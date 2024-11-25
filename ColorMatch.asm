; ColorMatch
; (C) 2024 Thomas Jentzsch

; Name ideas:
; - Chameleon!, ...World, Life, Day, Quest, Run, Crisis, Command, Chaos

; Ideas:
; - game play:
;   + remove found cells, either block or fall into
;   + swap found cells
;   + swap random cells/rows/columns
;   ? remove random cells (blocking), keep path?
;   x automatically move whole screen and/or random/specific rows/columns
;   ? require a button press, indicated with ? or ! on central sprite
;   o multiple levels
;     - 1 ordered
;     - 2 roll columns with time
;     - 2 roll rows with time
;     - 2 swap rows with time
;     - 3 swap columns with time
;     - 4 initially rolled/swapped rows
;     - 5 initially rolled/swapped columns
;     - 6 remove found cells (non blocking)
;     - 7 remove found cells (blocking)
;     - 8 swap found cells
;     - 9 remove found cells (deadly)
;     -10 require confirmation
; - reduce to
;   x 11 columns to get one sprite back
;   + 9 x 7 large cells for two sprites
;     + single player (with shading)
;     + two players without shading
; - 2-player mode
;   - display score alternating (last scoring player, different colors)
;   - display energy alternating (last scoring player/time based, e.g. every 1s)
;   - same or different targets?
;   -

; TODOs:
; - better randomization
; o better game over display (also remove target bars)
; - let chameleon adapt to current cell (drains time)
; - display adapted chameleon
; ? while adapting chameleon cannot move
; - make code 2 player ready (loop over X)

; DONEs:
; + game variations:
;   + ordered colors
;   + random row order
;   + random column order
;   + random cells
; + energy, bonus score when found fast
; + energy bar below main kernel
; + score position below
; + hue delta tbl
; + round progress bar
; + swap rows/cols
; + lose life energy based on color difference ("Chameleon")
; + pause at end of round
; + game over (with sound)
; + use RESET & SELECT switches
; + blank cell when leaving
;   x what happens in between? cell could have been moved away
;   x only block if player was able to move
;   + if blank cell immediately
;     x what if a different cell gets moved into?
;     x what if it is empty?
;     + ignore both cases
; + check round bonus score conversion
; + improve progess bar (6 digits + bar)
; + improve remaining cells display (colored ball)
; + overlap bottom most target cursor with bar preparation
; + change controls (move players instead of cells)

START_GAME      = 0
START_ROUND     = 0 ; NUM_ROUNDS-1

;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

VERSION         = $0040
BASE_ADR        = $f000

  IFNCONST TV_MODE  ; manually defined here
NTSC            = 0
PAL60           = 1
PAL50           = 0
  ENDIF

F8SC            = 1 ; create F8SC instead of 4KSC (for Harmony)

ILLEGAL         = 1
DEBUG           = 1

BLOCK_CELLS     = 1

SAVEKEY         = 0 ; (-~220) support high scores on SaveKey
PLUSROM         = 0 ; (-~50)

RM_LEAD_0       = 1


;===============================================================================
; I N C L U D E S
;===============================================================================

    processor 6502
  LIST OFF
    include vcs.h
    include tv_modes.h
    include jtz_macros.asm
  LIST ON


;===============================================================================
; C O L O R - C O N S T A N T S
;===============================================================================

  IF NTSC_COL
BLACK2              = BLACK
  ELSE
BLACK2              = BLACK+2   ; black is darker in PAL
  ENDIF

ENERGY_COL          = GREEN+$8  ;BLUE_CYAN+$c
ENERGY_CX_COL       = RED+$e    ;BLUE_CYAN+$6
NO_ENERGY_COL       = BLACK2+$4
NO_ENERGY_CX_COL    = RED+$8    ;BLACK2+$2

EMPTY_COL           = BLACK;+$1


;===============================================================================
; G A M E - C O N S T A N T S
;===============================================================================

EOR_RND_LO      = $bd           ; %10110100 ($9c, $b4, $bd, $ca, $eb, $fc)

NUM_ROWS        = 7
NUM_COLS        = 9
NUM_CELLS       = NUM_ROWS * NUM_COLS ; = 63
ROW_MASK        = $07
COL_MASK        = $0f
CELL_MASK       = $3f

CELL_H          = 21
BAR_HEIGHT      = 4

NUM_PLAYERS     = 1

MOVE_SPEED      = $20
OBST_SPEED      = (256 * 8) / (2 * 60)      ; every 2 seconds

MAX_ENERGY      = 160-8                                             ; 152
CELL_BONUS      = (MAX_ENERGY * 20 + 50) / 100                      ; 20%
ROUND_BONUS     = (MAX_ENERGY * 50 + 50) / 100                      ; 50%
;ENERGY_SPEED     = (256 * MAX_ENERGY + (60 * 25) / 2) / (60 * 25)  ; 25 seconds
ENERGY_SPEED    = (256 * MAX_ENERGY + (60 * 60) / 2) / (60 * 60)    ; 60 seconds
BURN_RATE       = (256 * MAX_ENERGY + (60 * 5) / 2) / (60 * 5)      ; 5 seconds
MOVE_TIME       = CELL_BONUS*256/(NUM_COLS+NUM_ROWS)                ; allow 7+9 steps (-time based losses) per target

NUM_DIGITS      = 3
DIGIT_BYTES     = NUM_DIGITS * 2
NUM_TMPS        = DIGIT_BYTES * 2 + 2  ; 14 TODO: reduce

; gameState flags:
GAME_RUNNING    = 1 << 7
ROUND_DONE      = 1 << 6
SWCHB_PRESSED   = 1 << 5
;...
IN_FOUND_CELL   = 1 << 2
SELECT_MODE     = 1 << 1
GAME_OVER       = 1 << 0

; roundFlags constants:
; any of these 5, values and their order must NOT be changed! (see apply obstacles)
SCROLL_ROWS     = 1 << 0    ; affects 13 cells
SCROLL_COLS     = 1 << 1    ; affects  9 cells
SWAP_FOUND      = 1 << 2    ; affects  2 cells (easiest)
SWAP_COLS       = 1 << 3    ; affects 18 cells
SWAP_ROWS       = 1 << 4    ; affects 26 cells
; SWAP and SCROLL mixed creates a mess!
; unused        = 1 << 5
; only one of these:
BURN_EMPTY      = 1 << 6
BLOCK_EMPTY     = 1 << 7

; chamState0|1 flags:
;CHAM_HIDDEN     = 1 << 7

STACK_SIZE      = 4 ; 2 only used in InitGame/NextRound


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

; preserved between games:
randomLo        .byte
randomHi        .byte

gameState       .byte           ; 1.......

variation       .byte           ; 0..3
round           .byte
roundFlags      .byte
;---------------------------------------
;hiScoreLst      ds DIGIT_BYTES / 2
;---------------------------------------
; reset per game:
frameCnt        .byte
;---------------------------------------
cellCntLst      ds  NUM_PLAYERS ; number of remaing cells per round
cellCnt0        = cellCntLst
;cellCnt1        = cellCntLst+1
chamColorLst    ds  NUM_PLAYERS
chamColor0      = chamColorLst
;chamColor1      = chamColorLst+1
chamStateLst    ds  NUM_PLAYERS
chamState0      = chamStateLst
;chamState1      = chamStateLst+1
targetColorLst  ds NUM_PLAYERS
targetColor0    = targetColorLst
;targetColor1    = targetColorLst+1
xPlayerLst      ds NUM_PLAYERS
xPlayer0        = xPlayerLst
;xPlayer1        = xPlayerLst+1
yPlayerLst      ds NUM_PLAYERS
yPlayer0        = yPlayerLst
;yPlayer1        = yPlayerLst+1
moveSumLst      ds NUM_PLAYERS  ; joystick directional input delay counter
moveSum0        = moveSumLst
;moveSum1        = moveSumLst+1
;---------------------------------------
energyLst       ds 2
energyLo        = energyLst
energyHi        = energyLst+1
; TODO: 2nd player
;---------------------------------------
scoreLst        ds DIGIT_BYTES / 2
scoreLo         = scoreLst
scoreMid        = scoreLst+1
scoreHi         = scoreLst+2
; TODO: 2nd player
;---------------------------------------
obstSum         .byte           ; obstacle delay counter

soundIdx0       .byte           ; pings
soundIdx1       .byte           ; burning, tick
;---------------------------------------
RAMKernel       ds KernelCodeEnd - KernelCode ; (48 bytes reserved for RAM kernel)
;---------------------------------------
tmpVars         ds NUM_TMPS     ; can overlap with stack
;---------------------------------------
RAM_END         = .

;===============================================================================
; S C - R A M - V A R I A B L E S
;===============================================================================

    SEG.U   SC_variables
    ORG     $F000

R_OFS           = $80

colorLst_W      ds  NUM_CELLS
colorLst_R      = colorLst_W + R_OFS

colorBuf_W      ds  NUM_ROWS
colorBuf_R      = colorBuf_W + R_OFS

    ECHO    "SC-RAM free:", [$F080 - .]d, "bytes"


;===============================================================================
; M A C R O S
;===============================================================================

DEBUG_BYTES SET 0

  MAC DEBUG_BRK
    IF DEBUG
DEBUG_BYTES SET DEBUG_BYTES + 1
      brk                         ;
    ENDIF
  ENDM

;  MAC NOP_IMM   ; skip 1 byte, 2 cycles
;    .byte   $82
;  ENDM
;
;  MAC NOP_B     ; skip 1 byte, 3 cycles
;    .byte   $04
;  ENDM

  MAC NOP_W     ; skip 2 bytes, 4 cycles
    .byte   $0c
  ENDM

;  MAC BIT_B     ; skip 1 byte, 3 cycles
;    .byte   $24
;  ENDM
;
;  MAC BIT_W     ; skip 2 bytes, 4 cycles
;    .byte   $2c
;  ENDM

  MAC START_TMP
   LIST OFF
; use default variable:
_ARGS SET {0}
   IF _ARGS = ""
_TMP_ORG SET tmpVars
   ELSE
_TMP_ORG SET _ARGS
   ENDIF

_END_TMP SET _TMP_ORG
  LIST ON
    SEG.U   TempVariables
    ORG     _TMP_ORG
  ENDM

  MAC CONT_TMP
    SEG.U   TempVariables
    ORG     _END_TMP
  ENDM

  MAC END_TMP
    LIST OFF
_END_TMP SET .
    IF . > tmpVars + NUM_TMPS
    LIST ON
      ECHO "ERROR: too many tmpVars!", . - tmpVars + NUM_TMPS
      ERR
    ENDIF
    LIST ON
    SEG     Bank0
  ENDM

  MAC KERNEL_CODE
    START_TMP tmpVars + DIGIT_BYTES + 4
.gfxPtr0    ds 2
.gfxPtr1    ds 2
    END_TMP
.loopKernel                     ;           @73
    lda     #$00                ; 2                 black r
    sta     COLUBK              ; 3 =  5    @02     black w
;ChamGfx0
;    lda     Chameleon,y         ; 4
    lda     (.gfxPtr0),y        ; 5
    sta     GRP0                ; 3         @10
;ChamGfx1
;    lda     Chameleon,y         ; 4
    lda     (.gfxPtr1),y        ; 5
    sta.w   GRP1                ; 4 = 17    @19
    cpy     #3                  ; 2 =  2
.contKernel0                    ;           @21
Col0
    lda     #$00                ; 2                 #0r
    sta     COLUBK              ; 3         @26     #0w
.enterKernel
Col1
    lda     #$00                ; 2                 #1r
    sta     COLUBK              ; 3 = 10    @31!    #1w
Col2
    lda     #$00                ; 2                 #2r
    sta     COLUBK              ; 3                 #2w
Col3
    lda     #$00                ; 2                 #3r
    sta.w   COLUBK              ; 3                 #3w
Col4
    lda     #$00                ; 2                 #4r
    sta     COLUBK              ; 4 = 16    @47!    #4w
Col5
    lda     #$00                ; 2                 #5r
    sta     COLUBK              ; 3                 #5w
Col6
    lda     #$00                ; 2                 #6r
    sta.w   COLUBK              ; 3         @58     #6w
Col7
    lda     #$00                ; 2                 #7r
    sta     COLUBK              ; 3                 #7w
Col8
    lda     #$00                ; 2                 #8r
    sta     COLUBK              ; 3 = 21    @68!    #8w
    dey                         ; 2
    bcs     .loopKernel         ; 3/2
    jmp     ExitKernel          ; 3         @75
KernelCodeEnd

ContKernel0 = RAMKernel + .contKernel0 - KernelCode
EnterKernel = RAMKernel + .enterKernel - KernelCode
PD = KernelCode - RAMKernel     ; patch delta
  ENDM


;===============================================================================
; R O M - C O D E
;===============================================================================
    SEG     Bank0
    ORG     BASE_ADR

    NEXT_PASS

    ds      256, $ff

;---------------------------------------------------------------
DrawKernel SUBROUTINE
;---------------------------------------------------------------
    START_TMP
.digitPtrLst    ds  12
.digitPtr4      = .digitPtrLst
.digitPtr2      = .digitPtrLst+2
.digitPtr0      = .digitPtrLst+4
.digitPtr5      = .digitPtrLst+6
.digitPtr3      = .digitPtrLst+8
.digitPtr1      = .digitPtrLst+10
.ballPtr        = $fc               ; TODO
    END_TMP

; *** Setup Digit Pointers ***
; setup digit pointers (fills the first 6 bytes of digitPtrLst):
TIM_DIGITS_START

    ldx     #DIGIT_BYTES/2-1
; setup progress bar:
    lda     cellCnt0
    lsr
    lsr
    adc     cellCnt0
    adc     #7
    lsr
    lsr
    lsr
    eor     #$ff
    clc
    adc     #<Blank
    sta     .ballPtr
  IF RM_LEAD_0
    bit     .digitPtrLst+11 ; must be a high pointer (with bit 6 set)
;    bit     $ffff
  ENDIF

.loopSetDigits
; 2/5, 1/4, 0/3
; setup high nibble:
    lda     scoreLst,x
    pha
    lsr
    lsr
    lsr
    lsr
    tay
  IF RM_LEAD_0
    bne     .skip0HiV
    bvc     .skip0Hi
    ldy     #ID_BLANK
    NOP_IMM
.skip0HiV
    clv
.skip0Hi
  ENDIF
    lda     DigitPtrTbl,y
    sta     .digitPtrLst,x
; setup low nibble:
    pla
    and     #$0f
    tay
  IF RM_LEAD_0
    bne     .skip0LoV
    txa                     ; last digit 0 is always displayed (was: but 0 is never displayed in game)
    beq     .skip0Lo
    bvc     .skip0Lo
    ldy     #ID_BLANK
    NOP_IMM
.skip0LoV
    clv
.skip0Lo
  ENDIF
    lda     DigitPtrTbl,y
.setDigitPtrLSB
    sta     .digitPtrLst+DIGIT_BYTES/2,x
; loop:
    dex
    bpl     .loopSetDigits
TIM_DIGITS_END
; 196 cycles

    START_TMP tmpVars + DIGIT_BYTES
.rowCount       ds 1
.colorPtr       ds 2
.tmpGfx1        ds 1
.gfxPtr0        ds 2
.gfxPtr1        ds 2
    END_TMP

    lda     #<colorLst_R + NUM_COLS * (NUM_ROWS - 1)
    sta     .colorPtr
    lda     #>colorLst_R
    sta     .colorPtr+1         ;  = 10

    lda     #>Chameleon
    sta     .gfxPtr0+1
    sta     .gfxPtr1+1

    lda     #%01000100
    sta     PF2
    sta     PF0
    lsr                         ;           #%00100010
    sta     PF1                 ;  = 13
    lsr
    sta     VDELP0              ;           enable

    ldy     #NUM_ROWS           ;           = 6
    sty     .rowCount

    lda     #%110101            ; 2                 double width players, 8 pixel ball (4 would do too)
    sta     NUSIZ0              ; 3
    sta     NUSIZ1              ; 3 = 10    @18

    jsr     PrepareTargetBars
    stx     GRP0                ;           VDELed!

    lda     .tmpGfx1           ; 4 = 10
.waitTim
    ldy     INTIM
    bne     .waitTim
    sty     VBLANK              ; 3
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    beq     .enterLoop          ; 3         @06
    DEBUG_BRK

;---------------------------------------------------------------
;.exitLoopJmp
;    jmp     .exitLoop

.bottom1                        ;           @02
    lda     #0                  ; 2
    sta     COLUBK              ; 3 =  5    @07
    lda     #%110101            ; 2                 double width players, 8 pixel ball (4 would do too)
    sta     NUSIZ0              ; 3
    sta     NUSIZ1              ; 3
    stx     GRP0                ; 3 = 11            VDELed
    jmp     ContKernel0         ; 3 =  3    @21

; kernel loops to here
ExitKernel                      ;           @75
    bne     .bottom1            ; 2/3               Idea: bmi could work here too
    sta     HMOVE               ; 3 =  5    @04
    sty     COLUBK              ; 3
; enable target cursor:         ;           @04
    lda     .tmpGfx1            ; 3
.enterLoop                      ;           @07
    sta     GRP1                ; 3 =  9
  IF NUM_PLAYERS = 2
    lda     targetColor1        ; 3
  ELSE
    lda     #0
  ENDIF
    sta     COLUP1              ; 3
    lda     targetColor0        ; 3
    sta     COLUP0              ; 3 = 12    @25!    (goal < @26)
; restore stack pointer:
    ldx     #$ff                ; 2
    txs                         ; 2
; loop:
    dec     .rowCount           ; 3
    bmi     .exitLoop;Jmp       ; 2/3
    ldy     .rowCount           ;  =  3
; calculate for new row:
    jsr     PrepareTargetBars   ;
    txs                         ; 2 = ???
; setup chameleon pointers:
    lda     #<NoChameleon       ; 2
  IF NUM_PLAYERS = 2
    cpy     yPlayer1            ; 3
    bne     .emptyP1            ; 2/3
    lda     #<Chameleon         ; 2
.emptyP1
  ENDIF
    sta     .gfxPtr1
;    sta     ChamGfx1 + 1 - PD   ; 3 = 11/12
    lda     #<NoChameleon       ; 2
    cpy     yPlayer0            ; 3
    bne     .emptyP0            ; 2/3
    lda     #<Chameleon         ; 2
;    bit     chamState0          ;           CHAM_HIDDEN?
;    bpl     .emptyP0            ;
;    lda     #<Chameleon0+1      ; 2
.emptyP0
    sta     .gfxPtr0
;    sta     ChamGfx0 + 1 - PD   ; 3 = 11/12
; move ball only:
    sta     HMCLR
    lda     #$50
    sta     HMBL                ;           @~40

;    ldx     targetColor0
;    lda     xPlayer0
;    cmp     xPlayer1
;    bne     .posOk
;    lda     yPlayer0
;    eor     yPlayer1
;    cmp     #1
;    bne     .posOk
;    ldx     targetColor1
;.posOk
; TODO: swap player assignment to GRPx/COLUPx
    sta     WSYNC
;---------------------------------------
    sta     HMOVE
    nop     COLUP0

; patch kernel with current line's color values:
    ldy     #NUM_COLS-1         ; 2 =  2
LoopPatch
    lda     (.colorPtr),y       ; 5
    ldx     PatchTbl,y          ; 4
    sta     $00,x               ; 4
    dey                         ; 2
    bpl     LoopPatch           ; 3/2=18/17
    CHECKPAGE LoopPatch
; 2 + 18 * 9 - 1 = 163

    lda     .colorPtr           ; 3
    sec                         ; 2
    sbc     #NUM_COLS           ; 2
    sta     .colorPtr           ; 3 = 10
; avoid moving ball AFTER kernel:
    ldx     #0                  ; 2
    stx     HMBL                ; 3
    lda     #$50                ; 2
    sta     HMP0                ; 3
    sta     HMP1                ; 3 = 13

    SLEEP   22
;    ldy     #4
;.loopWait
;    dey
;    bne     .loopWait

;    lda     #BLACK              ; 2
;    bit     chamState0          ; 3                 CHAM_HIDDEN?
;    bpl     .notHidden0         ; 2/3
;    NOP_W                       ; 1
;.notHidden0
                                ;           @62
  IF NUM_PLAYERS = 2
    ldy     chamColor1          ; 3
    stx     GRP0                ; 3                 VDELed! X = 0
  ELSE
    ldy     #0
    stx.w   GRP0                ; 3                 VDELed! X = 0
  ENDIF
    lda     Col0 + 1 - PD       ; 3
    stx     NUSIZ0              ; 3         @74     @>=73!
;---------------------------------------
    sta     COLUBK              ; 3 = 15    @01
    lda     chamColor0          ; 3
    stx     NUSIZ1              ; 3
    sta     COLUP0              ; 3
    sty     COLUP1              ; 3
    stx     GRP1                ; 3 = 15
    sta     HMOVE               ; 3 =  3    @19!    +5
    tsx                         ; 2
    ldy     #CELL_H             ; 2
    jmp     EnterKernel         ; 3 =  7    @26!

;---------------------------------------------------------------
.exitLoop                       ;           @37

; *** Draw energy bar ***
    START_TMP tmpVars + DIGIT_BYTES
.pf0a       ds 1
.pf1a       ds 1
.pf2a       ds 1
.pf2b       ds 1
.pf1b       ds 1
.pf0b       ds 1
    END_TMP
.energyCol      = $ff           ; TODO
.noEnergyCol    = $ff-1

; prepare energy bar (1/2):
    sta     WSYNC
;---------------------------------------
; setup energy colors:
    ldy     yPlayer0
    lda     RowOfsTbl,y
    clc
    adc     xPlayer0
    tay
; check if player is in empty, burning cell:
    lda     #NO_ENERGY_COL      ; 2
    ldx     colorLst_R,y
    bne     .energyStdCol       ; 3/2
    lda     gameState
    and     #IN_FOUND_CELL
    bne     .energyStdCol       ; 3/2
    lda     roundFlags
    and     #BURN_EMPTY
    beq     .energyStdCol
    lda     #NO_ENERGY_CX_COL   ; 2
    ldy     #ENERGY_CX_COL      ; 2
    NOP_W
.energyStdCol
    ldy     #ENERGY_COL         ; 2
    sta     .noEnergyCol        ; 3
    sty     .energyCol          ; 3
    sta     WSYNC
;---------------------------------------
    ldx     #%000               ; 2
    lda     #%100001            ; 2         quad size ball, reflected PF
    sta     CTRLPF              ; 3
    lda     #$70                ; 2         -7
    sta     HMCLR               ; 3
    sta     HMP1                ; 3
    lda     energyHi            ; 3
    lsr                         ; 2
    lsr                         ; 2
    tay                         ; 2
    lda     #$ff                ; 2 = 11
    cpy     #3                  ; 2
    bcc     .below3             ; 2/3
    stx     .pf0a               ; 3         X = 0
    cpy     #11                 ; 2
    bcc     .below11            ; 2/3
    stx     .pf1a               ; 3
    cpy     #19                 ; 2
    bcc     .below19            ; 2/3
    stx     .pf2a               ; 3
    cpy     #27                 ; 2
    bcc     .below27            ; 2/3
    stx     .pf2b               ; 3
    cpy     #35                 ; 2
    bcc     .below35            ; 2/3
    stx     .pf1b               ; 3
    lda     EnergyF-35,y        ; 4
    bcs     .setPF0b            ; 3 = 56

.below11                        ;   @23
    ldx     EnergyF-3,y         ; 4
    stx     .pf1a               ; 3
    bcc     .setPF2a            ; 3

.below19
    ldx     EnergyR-11,y        ; 4
    stx     .pf2a
    bcc     .setPF2b

.below27
    ldx     EnergyF-19,y        ; 4
    stx     .pf2b
    bcc     .setPF1b

.below35
    ldx     EnergyR-27,y        ; 4
    stx     .pf1b               ; 3
    bcc     .setPF0b            ; 3

.below3
    ldx     EnergyR+5,y         ; 4
    stx     .pf0a
    sta     .pf1a
.setPF2a
    sta     .pf2a
.setPF2b
    sta     .pf2b
.setPF1b
    sta     .pf1b
.setPF0b
    tay                         ; 2 =  2    @..65

    sta     WSYNC
;---------------------------------------
    sta     WSYNC
;---------------------------------------
    ldx     #0                  ; 2
    stx     GRP0                ; 3
    lda     energyHi            ; 3
    clc                         ; 2
    adc     #6                  ; 2
    sta     WSYNC               ; 3 = 32    @52
;---------------------------------------
    sta     RESP1               ; 3         prepare border sprite for energy bar
    stx.w   GRP1                ;           clear P0 & P1
WaitBar
    sbc     #$0f                ; 2
    bcs     WaitBar             ; 2/3
    CHECKPAGE WaitBar
    tax                         ; 2
    lda     HmTbl,x             ; 4
    sta     HMBL                ; 3
    sta     RESBL               ; 3         @23..73!
    sta     WSYNC               ; 3
;---------------------------------------
    sta     HMOVE               ; 3
    ldx     #$00                ; 2 =  5
    stx     COLUP1
    stx     GRP1
    stx     GRP0                ;           clear P0 & P1
    stx     NUSIZ1              ; 2
    dex
    stx     GRP1                ; 3

; pre-prepare digit kernel:
    lda     #%011               ; 2
    sta     NUSIZ0              ; 3
    lda     #$0e                ; 2         TODO? make variable?
    sta     COLUP0              ; 3

    ldx     #BAR_HEIGHT         ; 3
.loopBar
    sta     WSYNC               ; 3 =  9
;---------------------------------------
    lda     .noEnergyCol        ; 3
    sta.w   COLUPF              ; 4
    lda     .energyCol          ; 3
    sta     COLUBK              ; 3 = 13
    lda     .pf0a               ; 3
    sta     PF0                 ; 3         @19
    lda     .pf1a               ; 3
    sta     PF1                 ; 3         @24
    lda     .pf2a               ; 3
    sta     PF2                 ; 3 = 18    @31

    sty     PF0                 ; 3 =  3    @34

    lda     .pf1b               ; 3
    sta     PF1                 ; 3 =  6    @40

    lda     .pf2b               ; 3
    dex                         ; 2
    sta     PF2                 ; 3 =  8    @48
    bne     .loopBar            ; 3/2

;---------------------------------------------------------------
; *** Draw score ***
                                ;           @50
    lda     #>DigitGfx          ; 2
    sta     .ballPtr+1          ; 3
    ldy     #$0e                ; 2
    lda     #$10|%011           ; 2
    sta     HMP1                ; 3
    stx     HMBL                ; 3         @60
    stx     PF2                 ; 3 = 18    @68
    stx     PF1                 ; 3         @71     @>=69
    stx     PF0                 ; 3         @74     @>=73
    sta     VDELP1              ; 3         @01     @>=76, disables GRP1!
;---------------------------------------
    sta     NUSIZ1              ; 3         @04
    sty     COLUP1              ; 3 =  9    @07
    stx     ENABL               ; 3
    stx     COLUBK              ; 3         @13
    lda     #BROWN|$e           ; 2
    sta     COLUPF              ; 3 = 11    @18

    ldx     #DIGIT_BYTES-1      ; 2
    ldy     #DIGIT_BYTES*2-2    ; 2 =  4
LoopMove
    lda     .digitPtrLst,x      ; 4
    sta     .digitPtrLst,y      ; 5
    dey                         ; 2
    dey                         ; 2
    dex                         ; 2
    bne     LoopMove            ; 3/2=18/17 (last byte already at right position)
    CHECKPAGE LoopMove
; total: 4+5*18-1=93                        @13+93 = @30

    sta     RESBL               ; 3         @38
    sta     RESP0               ; 3         @41
    sta     RESP1               ; 3  = 9    @44

    lda     #>DigitGfx          ; 2
    sta     .digitPtr0+1        ; 3
    sta     .digitPtr1+1        ; 3
    sta     .digitPtr2+1        ; 3
    sta     .digitPtr3+1        ; 3
    sta     .digitPtr4+1        ; 3
    sta     .digitPtr5+1        ; 3
    ldy     #FONT_H-1           ; 2 = 22    @66
LoopScore                       ;           @66
    lax     (.digitPtr0),y      ; 5
    lda     (.ballPtr),y        ; 5 = 10    @76!
;---------------------------------------
    sta     HMOVE               ; 3
    stx     GRP0                ; 3
    sta     ENABL               ; 3
    lda     (.digitPtr1),y      ; 5
    sta     GRP1                ; 3
    lda     (.digitPtr2),y      ; 5
    sta     GRP0                ; 3 = 25
    lax     (.digitPtr5),y      ; 5
    txs                         ; 2
    lax     (.digitPtr3),y      ; 5
    lda     (.digitPtr4),y      ; 5
    stx     GRP1                ; 3 = 20    @45!
    sta     GRP0                ; 3         @48!
    tsx                         ; 2
    stx     GRP1                ; 3         @53!
    sta     GRP0                ; 3         @56!
    sta     HMCLR               ; 3
    tya
    dey                         ; 2
    bpl    LoopScore            ; 3/2=19    @65/64
    CHECKPAGE LoopScore
; 8 * 7 = 56 + 3 (GRP0) + 6 (move) + 4 (tsx/txs) + 5 (loop) + 2 (nop/typ) = 76
    sta     ENABL               ; 3
    sta     GRP0                ; 3
    sta     GRP1                ; 3
    sta     GRP0                ; 3
;    sta     VDELP0              ; 3
    sta     VDELP1              ; 3
    sta     COLUPF              ; 3 = 18    @04

    lda     #2
    sta     VBLANK
    ldx     #$ff
    txs
    jmp     ContKernel
; /DrawKernel

;---------------------------------------------------------------
PrepareTargetBars ;SUBROUTINE
;---------------------------------------------------------------
; target bars are drawn above and below player's row
  IF NUM_PLAYERS = 2
    tya                         ; 2         6..0
    sec                         ; 2         7, 6 = 1; 6, 6 = 1;
    sbc     yPlayer1            ; 3         6..0
    cmp     #$02                ; 2
    ldx     #0                  ; 2
    bcs     .skipP1             ; 2/3
; show target bar for P1:
    dex                         ; 2         %11111111
.skipP1
  ELSE
    ldx     #0
  ENDIF
    stx     .tmpGfx1

    tya                         ; 2
    sec                         ; 2
    sbc     yPlayer0            ; 3
    cmp     #$02                ; 2
    ldx     #0                  ; 2
    bcs     .skipP0             ; 2/3
; show target bar for P0:
    dex                         ; 2         %11111111
.skipP0
    rts
; /PrepareTargetBars


KernelCode ; copied into RAM and patched there
    KERNEL_CODE

    ALIGN_FREE_LBL   256, "PatchTbl"

PatchTbl
    .byte   Col0 + 1 - PD
    .byte   Col1 + 1 - PD
    .byte   Col2 + 1 - PD
    .byte   Col3 + 1 - PD
    .byte   Col4 + 1 - PD
    .byte   Col5 + 1 - PD
    .byte   Col6 + 1 - PD
    .byte   Col7 + 1 - PD
    .byte   Col8 + 1 - PD
    CHECKPAGE_DATA PatchTbl
NUM_COLS    = . - PatchTbl ; 9

;---------------------------------------------------------------
Start SUBROUTINE
;---------------------------------------------------------------
    lda     #0
    tax
    cld                     ; clear BCD math bit
.clearLoop
    dex
    txs
    pha
    bne     .clearLoop

    jsr     GameInit

MainLoop
;---------------------------------------------------------------
VerticalBlank SUBROUTINE
;---------------------------------------------------------------
    lda     #%1110          ; each '1' bits generate a VSYNC ON line (bits 1..3)
.loopVSync
    sta     WSYNC           ; 1st '0' bit resets Vsync, 2nd '0' bit exits loop
    sta     VSYNC
    lsr
    bne     .loopVSync      ; branch until VSYNC has been reset

    inc     frameCnt

  IF NTSC_TIM
    lda     #44+1-7        ; 38*64 = 2432
  ELSE
    lda     #77
  ENDIF
    sta     TIM64T

;---------------------------------------------------------------
; Check Switches and Fire Button
;---------------------------------------------------------------
    lax     SWCHB
    and     #%11
    cmp     #%11
    beq     .notPressed
    lda     #SWCHB_PRESSED
    bit     gameState
    bne     .skipSWCHB
    sta     gameState
    txa
    lsr
    bcs     .skipReset
    ldy     variation
    jsr     Reset
    bne     .contStopped
    DEBUG_BRK

.skipReset
;    lsr
;    bcs     .skipSelect
    ldy     variation
    iny
    cpy     #NUM_VARS
    bcc     .roundOk
    ldy     #0
.roundOk
    jsr     Select
    bne     .skipSWCHB
    DEBUG_BRK

.notPressed
    lda     gameState
    and     #~SWCHB_PRESSED
    sta     gameState
.skipSWCHB

    bit     gameState                   ; GAME_RUNNING?
    bmi     .checkInput
    lda     soundIdx0
    bne     .contStopped
    bvs     .roundDone                  ; ROUND_DONE!
    ldx     INPT4
    bmi     .contStopped
    lda     gameState
    lsr
    ror                                 ; SELECT_MODE? (2)
    bcs     .startGame                  ;  yes, start game
    bpl     .startNextRound             ; GAME_OVER? (1) no, start next round
    jsr     Reset                       ;  no, reset
    bne     .contStopped
    DEBUG_BRK

.startGame
    jsr     StartGame
    bne     .startNextRound
    DEBUG_BRK

.roundDone
    jsr     NextRound
    lda     #0
    sta     gameState
    beq     .contStopped

.startNextRound
    lda     #GAME_RUNNING
    sta     gameState
    jsr     GetTargetColorIdx   ; extra stack usage only here!
    sta     targetColor0
;    jsr     GetTargetColorIdx   ; extra stack usage only here!
;    sta     targetColor1
.contStopped
    jmp     .skipRunning

;    ldx     #NUM_PLAYERS-1
;.loopPlayers
;---------------------------------------------------------------
; Handle Joystick Directions
;---------------------------------------------------------------
.checkInput
    START_TMP
;.tmpSwchA   ds 1
.tmpXPlayer     ds 1
.tmpYPlayer     ds 1
    END_TMP
TIM_S

    lax     SWCHA
; control repeat rate
    cmp     #$ff
    lda     moveSum0
    bcc     .dirPressed
    lda     #MOVE_SPEED-1       ; reset move delay
.dirPressed
    sbc     #MOVE_SPEED-1
    sta     moveSum0
    bcs     .skipDirs
; move player:
    txa
    ldy     xPlayer0
    asl
    bcs     .skipRight
    iny
    cpy     #NUM_COLS
    bcc     .setXPlayer
    ldy     #0
    bpl     .setXPlayer

.skipRight
    bmi     .skipLeft
    dey
    bpl     .setXPlayer
    ldy     #NUM_COLS-1
.setXPlayer
.skipLeft
    sty     .tmpXPlayer
    ldy     yPlayer0
    asl
    asl
    bcs     .skipDown
    dey
    bpl     .setYPlayer
    ldy     #NUM_ROWS-1
    bpl     .setYPlayer

.skipDown
    bmi     .skipUp
    iny
    cpy     #NUM_ROWS
    bcc     .setYPlayer
    ldy     #0
.setYPlayer
.skipUp
    sty     .tmpYPlayer
  IF BLOCK_CELLS
    lda     roundFlags                  ;           BLOCK_FLAG?
    bpl     .skipBlockR
    lda     RowOfsTbl,y
    clc
    adc     .tmpXPlayer
    tay
    lda     colorLst_R,y
    beq     .skipMove
.skipBlockR
  ENDIF
    lda     .tmpXPlayer
    sta     xPlayer0
    lda     .tmpYPlayer
    sta     yPlayer0
    ldy     #>MOVE_TIME
    lda     #<MOVE_TIME
    jsr     DecreaseEnergy
.skipMove
;    lda     #0
;    sta     chamState0
    lda     #GAME_RUNNING       ; remove IN_FOUND_CELL flag
    sta     gameState
.skipDirs

.skipRunning

    bit     INPT5
    bmi     .skipResetColors
    lda     frameCnt
    and     #$1f
    bne     .skipResetColors
    jsr     InitColors
.skipResetColors

; position players and ball:
    lda     xPlayer0
    ldx     #0
    jsr     SetXPosTarget
  IF NUM_PLAYERS = 2
    lda     xPlayer1
    inx
    jsr     SetXPosTarget
  ENDIF
    sta     WSYNC
;---------------------------------------
    sta     RESBL
    lda     #$62
    sta     HMBL
    sta     ENABL
    lda     #%110000
    sta     CTRLPF
;    sta     WSYNC
;;---------------------------------------
;    lda     #%10
;    sta     ENABL
;    lda     #%110000
;    sta     CTRLPF
;    SLEEP   57
;    sta     RESBL

; /VerticalBlank

    jmp     DrawKernel
ContKernel

;---------------------------------------------------------------
OverScan SUBROUTINE
;---------------------------------------------------------------
  IF NTSC_TIM
    lda     #36-1-9
  ELSE
    lda     #63
  ENDIF
    sta     TIM64T

TIM_OVS
    lda     gameState                   ; GAME_RUNNING?
    bmi     .contRunning
    jmp     .skipRunning

.contRunning
;---------------------------------------------------------------
; Decrease Energy
;---------------------------------------------------------------
    START_TMP
.tmpColorIdx    ds 1        ; for clearing cell
.tmpColor       ds 1        ; for difference calculation and empty cell
.tmpDiff        ds 1
.hueDiff        ds 1
.valDiff        ds 1
TMP_BASE_PLR    = .
;.tmpVal         = .tmpDiff
    END_TMP

; get cell-index and -color:
    ldy     yPlayer0
    lda     RowOfsTbl,y
    clc
    adc     xPlayer0
    sta     .tmpColorIdx
    tay
    lda     colorLst_R,y                ; EMPTY_COL?
    sta     .tmpColor
; calculate hue-diff:
    lsr
    lsr
    lsr
    lsr
    tay
    lda     targetColor0                ; chamColor0
    lsr
    lsr
    lsr
    lsr
    tax                                 ; TODO: replace for 2-player code
    lda     HueIdx,x
    sec
    sbc     HueIdx,y
    bcs     .posHueDiff
    eor     #$ff
    adc     #1
.posHueDiff
    cmp     #NUM_COLS/2+1
    bcc     .hueOk
    eor     #$ff
    adc     #NUM_COLS+1-1
.hueOk
    sta     .hueDiff
; calculate value-diff:
   IF SKIPPED_VAL = 1
; use value-diff table if values are skipped
    lda     targetColor0
    and     #$0f
    lsr
    tay
    lda     ValIdxTbl,y
    pha
    lda     .tmpColor
    and     #$0f
    lsr
    tay
    pla
    sec
    sbc     ValIdxTbl,y
    bcs     .posValDiff
    eor     #$ff
    adc     #1
.posValDiff
   ELSE ;{
    lda     .tmpColor
    and     #$0f
    lsr
    sta     .tmpVal
    lda     targetColor0                ; chamColor0
    and     #$0f
    sec
    sbc     .tmpVal
    bcs     .posValDiff
    eor     #$ff
    adc     #1
.posValDiff
    lsr
.wait4ever                              ; DEBUG, remove!
    bcs     .wait4ever
   ENDIF ;}
    sta     .valDiff
; calculate energy-speed:
    lda     .tmpColor                   ; EMPTY_COL?
    bne     .calcDiff                   ;  no, normal calculation
    bit     roundFlags                  ; BURN_EMPTY?
    bvc     .calcDiff0                  ;  no, used fixed energy rate
    lda     gameState                   ; in found cell?
    and     #IN_FOUND_CELL              ;
    bne     .calcDiff0                  ;  yes, used fixed energy rate
    lda     soundIdx1
    bne     .skipBurnSound
    sta     AUDF1
    lda     #$4
    sta     AUDV1
    lda     #$08
    sta     AUDC1
    inc     soundIdx1
.skipBurnSound
    lda     #BURN_RATE
    bne     .contDecEnergy

; calculate hue difference:
.calcDiff0
    lda     #ENERGY_SPEED
    bne     .contDecEnergy

.calcDiff
    lda     .valDiff
    clc
    adc     .hueDiff                    ; 1..4 + 1..6 = 2..10 (was: 1..6 + 1..7 = 2..13)
; multiply diff by 6:
;    sta     .tmpDiff
    asl                                 ; 4..20 (was: 4..26)
;    adc     .tmpDiff                    ; 6..30
;    adc     .hueDiff
    asl                                 ; 12..60 (was: 8..52)
    adc     #ENERGY_SPEED
.contDecEnergy
    ldy     #0
    jsr     DecreaseEnergy
;---------------------------------------------------------------
; Check for match
;---------------------------------------------------------------
; check for color match (TODO: can be done every 2nd frame)
    lda     .hueDiff
    clc
    adc     .valDiff
    beq     .foundTargetCol
    bit     SWCHB
    bmi     .skipMatch
    cmp     #2                          ;  difference < 2?
    bcs     .skipMatch                 ;  no, no match
.foundTargetCol
    lda     roundFlags
    and     #BLOCK_EMPTY|BURN_EMPTY
    beq     .skipEmpty
    ldy     .tmpColorIdx
    lda     #EMPTY_COL
    sta     colorLst_W,y
    lda     #GAME_RUNNING|IN_FOUND_CELL
    sta     gameState
.skipEmpty
; update colors for next target
;    lda     targetColor0
;    sta     chamColor0
;    lda     #CHAM_HIDDEN
;    sta     chamState0
; TODO: compare with current target color
    jsr     GetTargetColorIdx
    sta     targetColor0
; increase score:
    lda     #$00
    sec
    jsr     AddScore
; start found sound and add extra time:
    lda     #$04
    sta     AUDC0
    ldx     #FOUND_SOUND_LEN            ; TODO: replace for 2-player code
    ldy     #$0e
    lda     #CELL_BONUS
    dec     cellCnt0
    bne     .skipNextRound
    lda     #ROUND_DONE                 ; remove GAME_RUNNING
    sta     gameState
    ldx     #BONUS_SOUND_LEN            ; TODO: replace for 2-player code
    ldy     #$0a
    lda     #ROUND_BONUS+CELL_BONUS     ; = 106
.skipNextRound
    stx     soundIdx0
    sty     AUDF0
;---------------------------------------------------------------
; Increase Energy
;---------------------------------------------------------------
    clc
    adc     energyHi                ; 152 + 152/2 + 152/5 = 258!
    bcs     .hexOverFlow
    cmp     #MAX_ENERGY
    bcc     .setEnergyHi
.hexOverFlow
    sbc     #MAX_ENERGY             ; 258 - 152 = 106!
    jsr     Hex2BCD
    jsr     AddScore
    lda     #MAX_ENERGY
.setEnergyHi
    sta     energyHi
.skipMatch
;    dex
;    bmi     .exitLoop
;    jmp     .loopPlayers

;---------------------------------------------------------------
; Apply Obstacles
;---------------------------------------------------------------
    lda     frameCnt                ; TODO: can be done every 2nd frame
    and     #$07
    beq     .doApplyObst
.skipApplyObstJmp
    jmp     .skipApplyObst

.doApplyObst
    START_TMP
.tmpObst        ds  1
TMP_BASE_OBST   = .             ; .tmpObst has to be preserved until the end of obstacle handling!
.obstLst        ds  NUM_OBST
    END_TMP
    lda     obstSum
    clc
    adc     #OBST_SPEED
    sta     obstSum
    bcc     .skipApplyObstJmp
;---------------------------------------------------------------
; Select Obstacle
;---------------------------------------------------------------
    lda     roundFlags
    and     #SCROLL_ROWS|SCROLL_COLS|SWAP_FOUND|SWAP_COLS|SWAP_ROWS ; %...54321
    beq     .skipApplyObstJmp
; play tick sound:
;    ldx     soundIdx1
;    bne     .skipTick
    ldx     #TICK_SOUND_LEN
    stx     soundIdx1
    ldx     #$08
    stx     AUDC1
    ldx     #$06
    stx     AUDV1
;.skipTick
; determine which obstacle is applied this time:
    sta     .tmpObst
    ldx     #0
    ldy     #NUM_OBST-1
.loopGetObst
    lsr     .tmpObst
    lda     #0
    bcc     .noObst
    lda     ObstTbl,y
    sta     .obstLst,x
    inx
.noObst
    dey
    bpl     .loopGetObst
; X = number of obstacles+1
    txa
    tay                         ; 1..5
    jsr     NextRandom
    sec
.loopSetObst
    adc     ObstRndDiff-1,y
    dex
    bcs     .loopSetObst
    lda     .obstLst,x
    sta     .tmpObst
;---------------------------------------------------------------
; Scroll Rows and Columns
;---------------------------------------------------------------
; 875 (before cleanup), 980, 1025, 1289, 1307, 1332, 1345, 1356, 1364
    and     #SCROLL_ROWS|SCROLL_COLS    ; must 1 and 2!
    bne     .doScroll
    jmp     .skipScrolls

.doScroll
; scroll either row or column:
; determine type and direction:
    tax                         ; 0..3; 1 = rows, 2 = cols
    jsr     NextRandom
    and     ScrollMask,x        ; 0..1, 2..3, 0..3; %01, %10, %11
    ora     DirOfs,x            ; 0, 2, 0
    tay
    lda     DirBits,y
    asl                         ; A = ????....
    tax
    bcc     .scrollRow
; determine col:
.randomCol
    jsr     NextRandom          ; TODO: improve
    and     #COL_MASK
    cmp     #NUM_COLS
    bcs     .randomCol
    cpx     #%11101110
    bne     .doVScroll          ; offset == A
;    clc
    adc     #NUM_CELLS - NUM_COLS - 1; offset == NUM_CELLS - NUM_COLS + A
    bne     .doVScroll

.scrollRow
; determine row:
    ldy     #2                  ; 2 tries, 9/16 ok, 7/16*9/16 ok
.randomRow
    jsr     NextRandom          ; TODO: improve
    and     #ROW_MASK
    cmp     #NUM_ROWS
    bcc     .validRow
    dey
    bne     .randomRow
    lsr
.validRow
    tay                         ; Y = row
    lda     RowOfsTbl,y
    cpx     #%01111110
    bne     .scrollLeft
    adc     #NUM_COLS-2
.scrollLeft
.doVScroll
    tay
;---------------------------------------------------------------
; Scroll Cells
;---------------------------------------------------------------
; X = direction, Y = cell offset
; load first cell:
    lda     colorLst_R,y
    pha
; get direction:
    txa
    ldx     #NUM_COLS-2                 ;           used in right and left
    asl
    bcs     .skipRight
;---------------------------------------------------------------
; move cells right:
.loopColsR
    lda     colorLst_R-1,y              ; 4
    sta     colorLst_W,y                ; 5
    dey                                 ; 2
    dex                                 ; 2
    bpl     .loopColsR                  ; 3/2=16/15
    bmi     .storeFirst                 ; 3

;---------------------------------------------------------------
.skipRight
    bmi     .skipLeft
; move cells left:
.loopColsL
    lda     colorLst_R+1,y              ; 4
    sta     colorLst_W,y                ; 5
    iny                                 ; 2
    dex                                 ; 2
    bpl     .loopColsL                  ; 3/2=16/15
    bmi     .storeFirst                 ; 3

;---------------------------------------------------------------
.skipLeft
; check for down:
    asl
    asl
    bcs     .skipDown
; move cells down:
;    clc
.loopRowsD
    lda     colorLst_R+NUM_COLS,y       ; 4
    sta     colorLst_W,y                ; 5
    tya                                 ; 2
    adc     #NUM_COLS                   ; 2
    tay                                 ; 2
    cpy     #NUM_CELLS-NUM_COLS         ; 2
    bcc     .loopRowsD                  ; 3/2=29/28
    bcs     .storeFirst

;---------------------------------------------------------------
.skipDown
; must be move cells up:
;    sec
.loopRowsU
    lda     colorLst_R-NUM_COLS,y       ; 4
    sta     colorLst_W,y                ; 5
    tya                                 ; 2
    sbc     #NUM_COLS                   ; 2
    tay                                 ; 2
    cpy     #NUM_COLS                   ; 2
    bcs     .loopRowsU                  ; 3/2=29/28;    clc
.storeFirst
    pla
    sta     colorLst_W,y
.skipScrolls

;---------------------------------------------------------------
; Swap Cells
;---------------------------------------------------------------
    START_TMP TMP_BASE_OBST
.xCell0     ds 1
    END_TMP
;    lda     roundFlags
    lda     .tmpObst
    and     #SWAP_FOUND
    beq     .skipSwapCells
    jsr     GetRandomColorIdx
    sty     .xCell0
.repeatCell1
    jsr     GetRandomColorIdx
    cpy     .xCell0
    beq     .repeatCell1
    ldx     .xCell0
    lda     colorLst_R,y
    pha
    lda     colorLst_R,x
    sta     colorLst_W,y
    pla
    sta     colorLst_W,x
.skipSwapCells

;---------------------------------------------------------------
; Swap Columns
;---------------------------------------------------------------
;    lda     roundFlags
    lda     .tmpObst
    and     #SWAP_COLS
    beq     .skipSwapCols
; swap cols:
    START_TMP TMP_BASE_OBST
.col0       ds 1
    END_TMP
; determine col 0:
    ldy     #2                  ; 2 tries, 9/16 ok, 7/16*9/16 ok
.randomCol0
    jsr     NextRandom          ; TODO: improve
    and     #COL_MASK
    cmp     #NUM_COLS
    bcc     .validCol0
    dey
    bne     .randomCol0
    lsr
.validCol0
    sta     .col0
    tax
; determine col 1:
    ldy     #1                  ; 2 tries, 9/16 ok, 7/16*9/16 ok
.randomCol1
    jsr     NextRandom          ; TODO: improve
    and     #COL_MASK
    cmp     #NUM_COLS
    bcc     .validCol1
    dey
    bpl     .randomCol1
    lsr
.validCol1
    cmp     .col0
    beq     .randomCol1
    tay
    clc
; TODO?: maybe use pointers here (faster but probably more code)
.loopSwapCols
    lda     colorLst_R,y
    pha
    lda     colorLst_R,x
    sta     colorLst_W,y
    pla
    sta     colorLst_W,x
    txa
    adc     #NUM_COLS
    tax
    tya
    adc     #NUM_COLS
    tay
    cmp     #NUM_CELLS
    bcc     .loopSwapCols
.skipSwapCols

;---------------------------------------------------------------
; Swap Rows
;---------------------------------------------------------------
;    lda     roundFlags
    lda     .tmpObst
    and     #SWAP_ROWS
    beq     .skipSwapRows
; swap rows
    START_TMP TMP_BASE_OBST
.row0       ds 1
.count      ds 1
    END_TMP
; determine row 0:
    ldy     #2                  ; 2 tries, 9/16 ok, 7/16*9/16 ok
.randomRow0
    jsr     NextRandom          ; TODO: improve
    and     #ROW_MASK
    cmp     #NUM_ROWS
    bcc     .validRow0
    dey
    bne     .randomRow0
    lsr
.validRow0
    sta     .row0
    tax
; determine row 1:
    ldy     #1                  ; 2 tries, 9/16 ok, 7/16*9/16 ok
.randomRow1
    jsr     NextRandom          ; TODO: improve
    and     #ROW_MASK
    cmp     #NUM_ROWS
    bcc     .validRow1
    dey
    bpl     .randomRow1
    lsr
.validRow1
    cmp     .row0
    beq     .randomRow1
    tay
    lda     RowOfsTbl,y
    tay
    txa
    lda     RowOfsTbl,x
    tax
    lda     #NUM_COLS-1
    sta     .count
.loopSwapRows
    lda     colorLst_R,y
    pha
    lda     colorLst_R,x
    sta     colorLst_W,y
    pla
    sta     colorLst_W,x
    inx
    iny
    dec     .count
    bpl     .loopSwapRows
.skipSwapRows

.skipApplyObst

.skipRunning
;---------------------------------------------------------------
; Handle Sounds
;---------------------------------------------------------------
; continue sound 0:
    ldy     soundIdx0
    beq     .skipSound0
    lda     VolumeTbl-1,y
    sta     AUDV0
    dec     soundIdx0
.skipSound0
; continue sound 1:
    ldy     soundIdx1
    beq     .skipSound1
;    ldy     #8
    dec     soundIdx1
    NOP_W
.skipSound1
    sty     AUDV1

TIM_OVE ; 1870
.waitOver
    lda     INTIM
    bne     .waitOver
    jmp     MainLoop

;*****************************  S U B R O U T I N E S  *****************************

;---------------------------------------------------------------
GetRandomColorIdx SUBROUTINE
;---------------------------------------------------------------
; uses:    A, Y
; returns: A = color, Y = index
.repeatRandom
    jsr     NextRandom
    and     #CELL_MASK
    cmp     #NUM_CELLS
    bcc     .cellOk
    lsr                     ; TODO: improve
.cellOk
    tay
    lda     colorLst_R,y    ; EMPTY_COL?
    beq     .repeatRandom
    rts

;---------------------------------------------------------------
GetTargetColorIdx SUBROUTINE
;---------------------------------------------------------------
; uses:    A, Y
; returns: A = color, Y = index
.repeatRandom
    jsr     NextRandom
    and     #CELL_MASK
; don't swap with nearby cells:
    cmp     #NUM_CELLS-NUM_COLS*2-2*2+1
    bcc     .cellOk
    lsr                     ; TODO: improve
.cellOk
    adc     #NUM_COLS+2
    adc     xPlayer0
    ldy     yPlayer0
    adc     RowOfsTbl,y
    cmp     #NUM_CELLS
    bcc     .cellOk2
    sbc     #NUM_CELLS
.cellOk2
    tay
    lda     colorLst_R,y    ; EMPTY_COL?
    beq     .repeatRandom
    rts

;---------------------------------------------------------------
DecreaseEnergy SUBROUTINE
;---------------------------------------------------------------
    START_TMP TMP_BASE_PLR
.subLo      ds 1
.subHi      ds 1
    END_TMP
    sta     .subLo
    sty     .subHi
    clc
    lda     energyLo
    sbc     .subLo
    sta     energyLo
    lda     energyHi
    sbc     .subHi
    beq     .gameOver
    bcs     .contGame
.gameOver
    ldy     #GAME_OVER|SELECT_MODE
    sty     gameState
; play end of game sound:
    ldy     #BONUS_SOUND_LEN
    sty     soundIdx0
    ldy     #$7
    sty     AUDC0
    ldy     #$14
    sty     AUDF0
    lda     #0
    sta     targetColor0
    sta     chamColor0
.contGame
    sta     energyHi
    rts

;---------------------------------------------------------------
GameInit SUBROUTINE
;---------------------------------------------------------------
    lda     #$0e
    sta     chamColor0
  IF NUM_PLAYERS = 2
    lda     #$ac
    sta     chamColor1
  ENDIF
    ldx     #KernelCodeEnd-KernelCode-1
.loopCopy
    lda     KernelCode,x
    sta     RAMKernel,x
    dex
    bpl     .loopCopy

    lda     INTIM
    ora     #%10
    sta     randomLo

    ldy     #START_GAME
Select
    sty     variation
Reset
    lda     #4
    sta     xPlayer0
    lda     #3
    sta     yPlayer0
  IF NUM_PLAYERS = 2
    lda     #0
    sta     xPlayer1
    lda     #NUM_ROWS
    sta     yPlayer1
  ENDIF

    ldx     VarStartRound,y
    stx     round
    inx
    txa
    jsr     Hex2BCD
    sta     scoreLo
    tay
    lda     #SWCHB_PRESSED|SELECT_MODE  ; SELECT_MODE && !GAME_RUNNING
    bne     .contInit

StartGame
    lda     #0                          ; !SELECT_MODE && !GAME_RUNNING
    tay
.contInit
    sty     scoreLo
    ldy     #0
    sty     scoreMid
    sty     scoreHi
    sty     energyLo
    ldy     #MAX_ENERGY
    sty     energyHi
    ldy     variation
    ldx     VarStartRound,y
    bpl     .contInit2

NextRound
    ldx     round
    inx
    cpx     #NUM_ROUNDS
    bcc     .roundOk
    ldx     #NUM_ROUNDS-4               ; repeat last 4 rounds
.roundOk
    lda     #0                          ; !SELECT_MODE && !GAME_RUNNING
.contInit2
    stx     round
    sta     gameState                   ; !GAME_RUNNING
    lda     #0
    sta     frameCnt
    sta     obstSum

InitColors
    START_TMP
.colorPtrW  ds 2
.lum        ds 1
.row        ds 1
    END_TMP

    lda     #>colorLst_W
    sta     .colorPtrW+1
    ldy     #NUM_ROWS-1
.loopRows
    sty     .row
    lda     RowOfsTbl,y
    sta     .colorPtrW
    ldx     LumTbl,y
    ldy     #NUM_COLS-1
.loopColumns
    txa                         ; 2
    ora     ColorTbl,y          ; 4
    sta     (.colorPtrW),y      ; 6
    dey                         ; 2
    bpl     .loopColumns        ; 3/2=17/16
    ldy     .row
    dey
    bpl     .loopRows
; 2202 cycles = ~29.0 scanlines ;

;    ldy     #NUM_ROWS-1
;;    ldx     #0
;.loopRows
;    sty     .row
;    ldx     RowOfsTbl,y
;    lda     LumTbl,y
;    sta     .lum
;    clc
;    ldy     #0
;_VAL SET 0
;  REPEAT NUM_COLS
;    lda     .lum                ; 2
;    ora     ColorTbl + _VAL,y   ; 4
;    sta     colorLst_W + _VAL,x ; 5
;_VAL SET _VAL + 1
;  REPEND
;;    txa
;;    adc     #NUM_COLS
;;    tax
;    ldy     .row
;    dey
;    bpl     .loopRows
;; 1639 cycles

    lda     #0
    sta     targetColor0
  IF NUM_PLAYERS = 2
    sta     targetColor1
  ENDIF

    ldy     round
    lda     RoundFlags,y
    sta     roundFlags
    lda     RoundLen,y
    sta     cellCnt0
    rts
; /GameInit (does not return 0)

;---------------------------------------------------------------
Hex2BCD SUBROUTINE
;---------------------------------------------------------------
; Hex2Bcd (good 0-99), 22 bytes, 26 cycles:
    tax                     ; 2         0..63
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    tay                     ; 2         0..3
    txa                     ; 2
    sed                     ; 2
    clc                     ; 2
    adc     BcdTbl,y        ; 4 = 22    @26
    cld
    rts

;---------------------------------------------------------------
AddScore0 SUBROUTINE
;---------------------------------------------------------------
    clc
AddScore
    ldy     #0
    bcc     .skipIny
    iny
    clc
.skipIny
    sed
    adc     scoreLo
    sta     scoreLo
    tya
    adc     scoreMid
    sta     scoreMid
    lda     #0
    adc     scoreHi
    sta     scoreHi
    cld
    rts

;---------------------------------------------------------------
NextRandom SUBROUTINE
;---------------------------------------------------------------
    lda     randomLo        ; 3
    lsr                     ; 2
    rol     randomHi        ; 5
    bcc     .skipEor        ; 2/3
    eor     #EOR_RND_LO     ; 2
.skipEor
    sta     randomLo        ; 3 = 16/17
    eor     randomHi        ; 3 = 19/20
    rts
; NextRandom

    ALIGN_FREE_LBL  256, "SetXPosTarget"

;---------------------------------------------------------------
SetXPosTarget SUBROUTINE
;---------------------------------------------------------------
    asl
    asl
    asl
    asl
    adc     #14-5           ; HMOVEd initally
;SetXPos
    sec
    sta     WSYNC
WaitObject:
    sbc     #$0f            ; 2
    bcs     WaitObject      ; 3/2
    CHECKPAGE WaitObject
    eor     #$07            ; 2
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    asl                     ; 2
    sta     HMP0,x          ; 4
    sta.wx  RESP0,x         ; 5     @23!
    rts
; SetXPos


;===============================================================================
; R O M - T A B L E S (Bank 0)
;===============================================================================
    ALIGN_FREE_LBL  256, "ROM Tables"

DigitGfx
Four
    .byte   %00000110
    .byte   %00000110
    .byte   %01111111
    .byte   %01111111
    .byte   %01100110
    .byte   %01110110
    .byte   %00110110
    .byte   %00111110
    .byte   %00011110
    .byte   %00011110
FONT_H = . - Four

Seven
    .byte   %00111000
    .byte   %00111000
    .byte   %00111000
    .byte   %00111000
    .byte   %00111100
    .byte   %00011110
    .byte   %00001111
    .byte   %00000111
;    .byte   %01111111
;    .byte   %01111111
Two
    .byte   %01111111
    .byte   %01111111
    .byte   %01111000
    .byte   %00111100
    .byte   %00011110
    .byte   %00001111
    .byte   %00000111
    .byte   %01100111
    .byte   %01111111
;    .byte   %00111110
Six
    .byte   %00111110
    .byte   %01111111
    .byte   %01110011
    .byte   %01110011
    .byte   %01111111
    .byte   %01111110
    .byte   %01110000
    .byte   %01110011
    .byte   %01111111
;    .byte   %00111110
Three
    .byte   %00111110
    .byte   %01111111
    .byte   %01100111
    .byte   %00000111
    .byte   %00011110
    .byte   %00011110
    .byte   %00000111
    .byte   %01100111
    .byte   %01111111
;    .byte   %00111110
Nine
    .byte   %00111110
    .byte   %01111111
    .byte   %01100111
    .byte   %00000111
    .byte   %00111111
    .byte   %01111111
    .byte   %01100111
    .byte   %01100111
    .byte   %01111111
;    .byte   %00111110
Eight
    .byte   %00111110
    .byte   %01111111
    .byte   %01100111
    .byte   %01100111
    .byte   %00111110
    .byte   %00111110
    .byte   %01100111
    .byte   %01100111
    .byte   %01111111
;    .byte   %00111110
Zero
    .byte   %00111110
    .byte   %01111111
    .byte   %01100011
    .byte   %01100011
    .byte   %01101011
    .byte   %01101011
    .byte   %01100011
    .byte   %01100011
    .byte   %01111111
;    .byte   %00111110
Five
    .byte   %00111110
    .byte   %01111111
    .byte   %01100111
    .byte   %00000111
    .byte   %00111111
    .byte   %01111110
    .byte   %01100000
    .byte   %01100000
    .byte   %01111111
    .byte   %01111111

One
    .byte   %00001100
    .byte   %00001100
    .byte   %00001100
    .byte   %00001100
    .byte   %00001100
    .byte   %00001100
    .byte   %00001100
    .byte   %00111100
    .byte   %00111100
    .byte   %00011100

ProgressBar
    ds  FONT_H, $02;%11111100
Blank
    ds  FONT_H, 0
  CHECKPAGE_DATA_LBL DigitGfx, "DigitGfx"

DigitPtrTbl
    .byte   <Zero,  <One,   <Two,   <Three, <Four
    .byte   <Five,  <Six,   <Seven, <Eight, <Nine
ID_BLANK = . - DigitPtrTbl
    .byte   <Blank

ProgressGfx
    ds  FONT_H, %1000100

RowOfsTbl
_IDX    SET 0
    REPEAT NUM_ROWS
    .byte  _IDX * NUM_COLS
_IDX    SET _IDX + 1
    REPEND

    .byte   "JTZ"

  IF NTSC_COL
ColorTbl
;ORANGE  = $30
;    .byte   YELLOW         ; $10
    .byte   BROWN           ; $20
;    .byte   ORANGE          ; $30  skipped
    .byte   RED             ; $40
    .byte   MAUVE           ; $50
;    .byte   VIOLET          ; $60
    .byte   PURPLE          ; $70
    .byte   BLUE            ; $80
    .byte   BLUE_CYAN       ; $90
    .byte   CYAN            ; $a0
;    .byte   CYAN_GREEN      ; $b0
    .byte   GREEN           ; $c0
;    .byte   GREEN_YELLOW    ; $d0
    .byte   GREEN_BEIGE     ; $e0
;;    .byte   BEIGE          ; $f0  skipped
NUM_COLS    = . - ColorTbl  ; 9
HueIdx = . - 2
;    .byte   $ff     ; YELLOW          $10
    .byte   0       ; BROWN           $20
    .byte   $ff     ; (ORANGE)        $30   skipped
    .byte   1       ; RED             $40
    .byte   2       ; MAUVE           $50
    .byte   $ff     ; VIOLET          $60   skipped
    .byte   3       ; PURPLE          $70
    .byte   4       ; BLUE            $80
    .byte   5       ; BLUE_CYAN       $90
    .byte   6       ; CYAN            $a0
    .byte   $ff     ; CYAN_GREEN      $b0   skipped
    .byte   7       ; GREEN           $c0
    .byte   $ff     ; GREEN_YELLOW    $d0   skipped
    .byte   8       ; GREEN_BEIGE     $e0
                    ; (BEIGE)         $f0
  ELSE ; PAL
PAL_WHITE   = BLACK+$10     ; $10
ColorTbl
    .byte   YELLOW          ; $20
    .byte   ORANGE          ; $40
    .byte   RED             ; $60
    .byte   MAUVE           ; $80
    .byte   VIOLET          ; $a0
;    .byte   PURPLE          ; $c0
    .byte   BLUE            ; $d0
;    .byte   BLUE_CYAN       ; $b0
    .byte   CYAN            ; $90
    .byte   CYAN_GREEN      ; $70
;    .byte   GREEN           ; $50
    .byte   GREEN_YELLOW    ; $30
;    .byte   BLACK           ; $10

NUM_COLS    = . - ColorTbl  ; 13
HueIdx = . - 2
;    .byte   $ff     ; PAL_WHITE       $10
    .byte   0       ; YELLOW          $20
    .byte   8       ; GREEN_YELLOW    $30
    .byte   1       ; ORANGE          $40
    .byte   $ff     ; GREEN           $50   skipped
    .byte   2       ; RED             $60
    .byte   7       ; CYAN_GREEN      $70
    .byte   3       ; MAUVE           $80
    .byte   6       ; CYAN            $90
    .byte   4       ; VIOLET          $a0
    .byte   $ff     ; BLUE_CYAN       $b0   skipped
    .byte   $ff     ; PURPLE          $c0   skipped
    .byte   5       ; BLUE            $d0
  ENDIF ; /PAL

LumTbl
  IF NTSC
SKIPPED_VAL = 1
    .byte   $00
    .byte   $02
    .byte   $04
    .byte   $06
    .byte   $08
    .byte   $0a
;    .byte   $0c    ; skipped
    .byte   $0e
   IF SKIPPED_VAL = 1
ValIdxTbl
    .byte   $00
    .byte   $01
    .byte   $02
    .byte   $03
    .byte   $04
    .byte   $05
    .byte   $ff     ; skipped
    .byte   $06
   ENDIF
  ELSE ;/NTSC
SKIPPED_VAL = 1
    .byte   $00
    .byte   $02
    .byte   $04
    .byte   $06     ; seems identical with $08 on Sony CRT
    .byte   $08
    .byte   $0a     ; seems identical with $0c on Sony CRT
;    .byte   $0c    ; skipped
    .byte   $0e
   IF SKIPPED_VAL = 1
ValIdxTbl
    .byte   $00
    .byte   $01
    .byte   $02
    .byte   $03
    .byte   $04
    .byte   $05
    .byte   $ff     ; skipped
    .byte   $06
   ENDIF
  ENDIF

EnergyR
    .byte   %11111110
    .byte   %11111100
    .byte   %11111000
    .byte   %11110000
    .byte   %11100000
    .byte   %11000000
    .byte   %10000000
    .byte   %00000000
EnergyF
    .byte   %01111111
    .byte   %00111111
    .byte   %00011111
    .byte   %00001111
    .byte   %00000111
    .byte   %00000011
    .byte   %00000001
    .byte   %00000000

; position table at very end of page:
    ORG_FREE_LBL . & ~$ff | ($100-16), "HmTbl"
HmPTbl
    .byte   $70
HmTbl = . - $f1
    .byte   $60, $50, $40, $30, $20, $10, $00
    .byte   $f0, $e0, $d0, $c0, $b0, $a0, $90, $80

    .byte   $55

ChameleonGfx = . - 1
Chameleon  = . - 1
    ds  (CELL_H-CHAMELEON_H+1)/2, 0
ChamGfxStart
    .byte   %00011000
    .byte   %01111110
    .byte   %11111111
    .byte   %10000001
    .byte   %11111111
    .byte   %11111111
    .byte   %01100110
    .byte   %01111110
    .byte   %01111110
    .byte   %10011001
    .byte   %10111101
    .byte   %10111101
    .byte   %10011001
    .byte   %01111110
    .byte   %00011000
CHAMELEON_H = . - ChamGfxStart
;    ds  (CELL_H-CHAMELEON_H)/2-1, 0
Chameleon0 = . - 1
    ds  (CELL_H-CHAMELEON_H+1)/2, 0
    .byte   %00011000
    .byte   %01100110
    .byte   %10000001
    .byte   %10111101
    .byte   %10000001
    .byte   %10000001
    .byte   %01011010
    .byte   %01000010
    .byte   %01000010
    .byte   %10000001
    .byte   %10100101
    .byte   %10100101
    .byte   %10000001
    .byte   %01100110
    .byte   %00011000
;    ds  (CELL_H-CHAMELEON_H)/2-1, 0
NoChameleon
    ds  CELL_H, 0
    CHECKPAGE ChameleonGfx

ObstTbl
    .byte   SWAP_ROWS, SWAP_COLS, SWAP_FOUND, SCROLL_COLS, SCROLL_ROWS
NUM_OBST = . - ObstTbl
ObstRndDiff
    .byte   0, ($101/2)-1, ($102/3)-1, ($103/4)-1
    .byte   ($104/5)-1
    ;ff 7f 55 3f 33
NUM_OBST = . - ObstRndDiff

ScrollMask = . - 1
    .byte   %01, %01, %11
DirOfs = . - 1
    .byte   %10, 0, 0
DirBits
    .byte   %11110111           ; column up
    .byte   %11101111           ; column down
    .byte   %01011111           ; row left
    .byte   %00111111           ; row right

;IndexTbl
;; TODO: adapt to players moving
;    .byte   NUM_CELLS/2-NUM_COLS-1
;    .byte   NUM_CELLS/2-NUM_COLS
;    .byte   NUM_CELLS/2-NUM_COLS+1
;    .byte   NUM_CELLS/2-1
;    .byte   NUM_CELLS/2
;    .byte   NUM_CELLS/2+1
;    .byte   NUM_CELLS/2+NUM_COLS-1
;    .byte   NUM_CELLS/2+NUM_COLS
;    .byte   NUM_CELLS/2+NUM_COLS+1
;INDEX_LEN = . - IndexTbl

VolumeTbl
    .byte   0
    ds      8, 1
    ds      4, 2
    ds      2, 3
    ds      2, 4
    .byte   5, 6, 7, 8, 9, 10
FOUND_SOUND_LEN = . - VolumeTbl
    ds      4, 2
    ds      2, 3
    ds      2, 4
    .byte   5, 6, 7, 8, 9, 10
    ds      4, 2
    ds      2, 3
    ds      2, 4
    .byte   5, 6, 7, 8, 9, 10
BONUS_SOUND_LEN = . - VolumeTbl

TICK_SOUND_LEN  = 1

BcdTbl  ; 7 entries (works up to 99)
    .byte   $00, $06, $12, $18, $24, $30, $36

  IF 1 ; {
RoundFlags
; TODO: define and order the rounds by difficulty
    .byte                                                                       ; 0     ordered
    .byte               SWAP_FOUND                                              ; 1     some mess
    .byte               SWAP_FOUND|SCROLL_COLS                                  ; 2     more mess
    .byte                                                  SWAP_COLS|SWAP_ROWS  ; 3     some order
    .byte                          SCROLL_COLS|SCROLL_ROWS                      ; 4     very messy
    .byte                                      SCROLL_ROWS|SWAP_COLS            ; 5     very messy
    .byte   BLOCK_EMPTY|SWAP_FOUND                                              ; 6     some mess
    .byte   BLOCK_EMPTY|SWAP_FOUND|SCROLL_COLS                                  ; 7     more mess
    .byte   BLOCK_EMPTY|SWAP_FOUND|            SCROLL_ROWS                      ; 8     more mess
    .byte   BLOCK_EMPTY|SWAP_FOUND|                        SWAP_COLS            ; 9     more mess
    .byte   BLOCK_EMPTY|SWAP_FOUND|                                  SWAP_ROWS  ;10     more mess
    .byte   BURN_EMPTY |SWAP_FOUND                                              ;11
    .byte   BURN_EMPTY |SWAP_FOUND|SCROLL_COLS                                  ;12
    .byte   BURN_EMPTY |SWAP_FOUND|            SCROLL_ROWS                      ;13
    .byte   BURN_EMPTY |SWAP_FOUND|                        SWAP_COLS            ;14
    .byte   BURN_EMPTY |SWAP_FOUND|                                  SWAP_ROWS  ;15
; TODO: random from here (1 x KEEP|BLOCK|BURN, 2..5 x others)
NUM_ROUNDS = . - RoundFlags
RoundLen
    .byte   12, 14, 16, 18, 20, 22
    .byte   25, 27, 29, 31, 33              ; BLOCK (good length?)
    .byte   37, 30, 43, 44, 47              ; BURN
NUM_ROUNDS = . - RoundLen
   ENDIF ;}
VarStartRound
    .byte   0, 3, 6, 11
NUM_VARS = . - VarStartRound

    .byte     " ColorMatch "
    VERSION_STR
  IF NTSC_COL
    .byte   " (NTSC)"
  ELSE
    .byte   " (PAL-60)"
  ENDIF
    .byte     " - (C) 2024 Thomas Jentzsch "

  IF F8SC
    ORG_FREE_LBL $fff0, "BS"
Start0
    bit     $fff8
    jmp     Start
    ds 4, 0
    .byte   "SC"
    .word   Start0
    .word   Start0
  ELSE
    ORG_FREE_LBL $fffa, "Vectors"
    .byte   "SC"        ; autodetect help
    .word   Start
    .word   VERSION
  ENDIF

  IF F8SC
    RORG $F000

    ds 4080, $ff
Start1
    bit     $fff8
    jmp     Start
    ds 4, 0
    .byte   "SC"
    .word   Start1
    .word   VERSION
  ENDIF


;===============================================================================
; O U T P U T
;===============================================================================

    LIST OFF
    ECHO ""
    ECHO "*** RAM-Kernel :", [KernelCodeEnd - KernelCode]d, "bytes ***"
    ECHO "*** Free RAM   :", [$100 - STACK_SIZE - RAM_END]d, "bytes ***"
    ECHO "*** Free ROM   :", [FREE_TOTAL + DEBUG_BYTES]d, "bytes ***"
    ECHO ""
    ECHO "*** Debug bytes:", [DEBUG_BYTES]d, "bytes ***"

  IF SAVEKEY
    ECHO ""
    ECHO "*** SaveKey enabled! ***"
  ENDIF
  IF PLUSROM
    ECHO ""
    ECHO "*** PlusROM enabled! ***"
  ENDIF

