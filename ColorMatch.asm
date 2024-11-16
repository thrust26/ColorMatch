; Color-Match
; (C) 2024 Thomas Jentzsch

; Bug: ScrollLeft, bottom row

; Ideas:
; - game play:
;   o remove found cells, either block or fall into
;   o swap found cells
;   - swap random cells/rows/columns
;   - remove random cells (blocking), keep path?
;   - automatically move whole screen and/or random/specific rows/columns
;   - require a button press, indicated with ? or ! on central sprite
;   - multiple levels
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

; - reduce to 11 columns to get one sprite back

; TODOs:
; - better randomization
; o reset switch
; - use SELECT
; - progress bar
; - pause at end of round
; - swap rows/cols

; DONEs:
; + game variations:
;   + ordered colors
;   + random row order
;   + random column order
;   + random cells
; + timer, bonus score when found fast
; + timer bar below main kernel
; + score position below
; + hue delta tbl


START_ROUND     = NUM_ROUNDS-1

;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

VERSION         = $0020
BASE_ADR        = $f000

  IFNCONST TV_MODE ; manually defined here
NTSC            = 0
PAL60           = 1
PAL50           = 0
  ENDIF

F8SC            = 1     ; create F8SC instead of 4KSC (for Harmony)

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
BLACK2          = BLACK
  ELSE
BLACK2          = BLACK+2
  ENDIF

TIMER_COL       = GREEN+$8  ;BLUE_CYAN+$c
TIMER_CX_COL    = RED+$e    ;BLUE_CYAN+$6
NO_TIMER_COL    = BLACK2+$4
NO_TIMER_CX_COL = RED+$8    ;BLACK2+$2

EMPTY_COL       = BLACK;+$1


;===============================================================================
; G A M E - C O N S T A N T S
;===============================================================================

EOR_RND_LO      = $bd           ; %10110100 ($9c, $b4, $bd, $ca, $eb, $fc)

NUM_ROWS        = 9
NUM_COLS        = 13
NUM_CELLS       = NUM_ROWS * NUM_COLS ; = 117
CELL_H          = 15-1
BAR_HEIGHT      = 4

MROUND_CELLS     = 20
MOVE_SPEED      = $20
OBST_SPPED      = $0c

MAX_TIMER       = 160-8                                         ; 152
CELL_BONUS      = (MAX_TIMER * 20 + 50) / 100                   ; 20%
ROUND_BONUS     = (MAX_TIMER * 50 + 50) / 100                   ; 50%
TIMER_SPEED     = (256 * MAX_TIMER + (60 * 25) / 2) / (60 * 25) ; 25 seconds

DIGIT_BYTES     = 6
NUM_TMPS        = DIGIT_BYTES * 2   ; 12

; gameState flags
GAME_RUNNING    = 1 << 7

; roundFlags constants:
SCROLL_ROWS     = 1 << 0
SCROLL_COLS     = 1 << 1
SWAP_ROWS       = 1 << 2    ; unused
SWAP_COLS       = 1 << 3    ; unused
SWAP_FOUND      = 1 << 4
KEEP_CELLS      = 1 << 5
BURN_EMPTY      = 1 << 6
BLOCK_EMPTY     = 1 << 7

STACK_SIZE      = 9


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

frameCnt        .byte
;---------------------------------------
tmpVars         ds NUM_TMPS
;---------------------------------------
randomLo        .byte
randomHi        .byte

gameState       .byte

round           .byte
roundFlags      .byte
cellCnt         .byte

targetCol       .byte

moveSum         .byte           ; joystick directional input delay counter
obstSum         .byte

soundIdx0       .byte
soundIdx1       .byte
;---------------------------------------
timerLst        ds 2
timerLo         = timerLst
timerHi         = timerLst+1
;---------------------------------------
scoreLst        ds DIGIT_BYTES / 2
scoreLo         = scoreLst
scoreMid        = scoreLst+1
scoreHi         = scoreLst+2
;---------------------------------------
RAMKernel       ds KernelCodeEnd - KernelCode ; (48 bytes reserved for RAM kernel)
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
_END_TMP SET .
    IF . > tmpVars + NUM_TMPS
      ECHO "ERROR: too many tmpVars!", . - tmpVars + NUM_TMPS
      ERR
    ENDIF
    SEG     Bank0
  ENDM

  MAC KERNEL_CODE
.loopKernel                     ;           @03
Col0
    lda     #$00                ; 2                 #0r
    sta     COLUPF              ; 3 =  5            #0w
    pla                         ; 4
    pha                         ; 3
    rol     $3f                 ; 5 = 12            free cycles (3|4 bytes)
Col3
    ldx     #$00                ; 2                 #3r
Col1
    lda     #$00                ; 2                 #1r
    sta     COLUPF              ; 3 =  7    @27!    #1w
Col2
    lda     #$00                ; 2                 #2r
    sta     COLUPF              ; 3                 #2w
    stx     COLUPF              ; 3                 #3w
Col4
    lda     #$00                ; 2                 #4r
    sta     COLUPF              ; 3                 #4w
Col6
    lda     #$00                ; 2                 #6r
    sta     COLUPF              ; 3 = 20    @47!    #6w
Col8
    ldx     #$00                ; 2                 #8r
Col7
    lda     #$00                ; 2                 #7r
    sta     COLUPF              ; 3                 #7w
    stx     COLUPF              ; 3                 #8w
Col10
    lda     #$00                ; 2                 #10r
    sta     COLUPF              ; 3                 #10w
Col11
    lda     #$00                ; 2                 #11r
Col12
    ldx     #$00                ; 2                 #12r
    sta     COLUPF              ; 3                 #11w
    dey                         ; 2
    stx     COLUPF              ; 3 = 25    @72!    #12w
; 13 blocks: 45 cycles               (9 * 3 + 8 * 2 = 43) (could be:  9 * 3 + 7 * 2 = 41)
;                                                         (1 sprite: 10 * 3 + 8 * 2 = 46)
; 11 blocks: 45-(12/3)*2 = 37 cycles (7 * 3 + 6 * 2 = 31) (could be:  7 * 3 + 5 * 2 = 29)
;                                    (8 * 3 + 6 * 2 = 36, 1 sprite)
.enterKernel
    sta     WSYNC               ; 3
;---------------------------------------
    bne     .loopKernel         ; 3/2
    rts                         ; 6         @08
KernelCodeEnd

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

    ds      3, 0

;---------------------------------------------------------------
DrawKernel SUBROUTINE
;---------------------------------------------------------------
    START_TMP
.digitPtrLst    ds  12
.digitPtr4  = .digitPtrLst
.digitPtr2  = .digitPtrLst+2
.digitPtr0  = .digitPtrLst+4
.digitPtr5  = .digitPtrLst+6
.digitPtr3  = .digitPtrLst+8
.digitPtr1  = .digitPtrLst+10
    END_TMP
    START_TMP tmpVars + DIGIT_BYTES
.rowCount   ds 1
.colorPtr   ds 2
.targetCol  ds 1
.tmpColP0   ds 1
.tmpColP1   ds 1
    END_TMP
.cursorPF2  = $fd   ; TODO

; *** Setup Digit Pointers ***
; setup digit pointers (fills the first 6 bytes of digitPtrLst):
TIM_DIGITS_START

    ldx     #DIGIT_BYTES/2-1
    lda     scoreLst,x
    pha
; setup progress bar
    lda     cellCnt
    clc
    adc     #7
    lsr
    lsr
    lsr
    eor     #$ff
    clc
    adc     #<Blank+1
  IF RM_LEAD_0
    bit     .digitPtrLst+11 ; must be a high pointer (with bit 6 set)
;    bit     $ffff
    bvs     .enterScoreLoop
  ELSE
    bne     .enterScoreLoop
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
.enterScoreLoop
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

    ldx     #%10110110
    lda     #BLACK
    bit     gameState
    bpl     .skipRunning
    lda     targetCol
    ldx     #%11110110
.skipRunning
    sta     .targetCol
    stx     .cursorPF2

    ldx     #227+1
.waitTim
    lda     INTIM
    bne     .waitTim
    sta     WSYNC
;---------------------------------------
    sta     VBLANK
    stx     TIM64T              ;  =  7

    lda     #%111               ; 2
    sta     NUSIZ0              ; 3
    sta     NUSIZ1              ; 3
    ldy     #NUM_ROWS-1         ; 2
    sty     .rowCount           ; 3 = 25
    lda     #%01100000          ; 2
    sta     PF0                 ; 3
    lda     #%11011011          ; 2
    sta     PF1                 ; 3
    asl                         ; 2         #%10110110
    SLEEP   7
    sta     RESP0               ; 3 = 10    @42
    ldx     #<colorLst_R + NUM_COLS * (NUM_ROWS - 1); 2
    sta     PF2                 ; 3
    stx     .colorPtr           ; 3
    ldx     #>colorLst_R        ; 2
    stx     .colorPtr+1         ; 3
    sta     RESP1               ; 3 = 16    @58

;---------------------------------------------------------------
.loopRows                       ;           @50/51
    clc                         ; 2
    ldy     #NUM_COLS-1         ; 2 =  4
LoopPatch
    lda     (.colorPtr),y       ; 5
    ldx     PatchTbl,y          ; 4
    sta     $00,x               ; 4
    dey                         ; 2
    bpl     LoopPatch           ; 3/2=21/20
    CHECKPAGE LoopPatch
; 4 + (21 - 3) * 13 - 1 = 276 - 39

    lda     .colorPtr
;    sec
    sbc     #NUM_COLS-1
    sta     .colorPtr

    lda     .tmpColP1
    ldx     #8
WaitGap
    dex
    bne     WaitGap             ;   = 41
    CHECKPAGE WaitGap
    ldy     #CELL_H
    sta.w   COLUP1
    lda     .tmpColP0
    sta     COLUP0
    lda     #%11000000
    sta     GRP0
    sta     GRP1                ;           @65
    jsr     EnterKernel         ;           @08
    sty     COLUPF              ;           Y = 0
    sty     GRP1
    lda     .targetCol
    sta     COLUP0
; enable target cursor:
    ldx     #%10110110
    lda     .rowCount
    eor     #NUM_ROWS/2
    lsr
    bne     .skipCursorWait
    ldy     #%00111100          ;           sprite for target color above and below central cell
    bcc     .skipCursor
    ldx     .cursorPF2          ;           = %11110110, wider central cell
.skipCursor
    sty     GRP0
    stx     PF2
    SLEEP   8
    dec     .rowCount
    bpl     .loopRows
    bmi     .exitKernel

.skipCursorWait
    nop
    bne     .skipCursor

.exitKernel
;    sta     WSYNC

;---------------------------------------------------------------
; *** Draw energy bar ***
    START_TMP tmpVars + DIGIT_BYTES
.pf0a       ds 1
.pf1a       ds 1
.pf2a       ds 1
.pf2b       ds 1
.pf1b       ds 1
.pf0b       ds 1
    END_TMP
.timerCol   = $fd
.noTimerCol = $fd-1

; prepare energy bar (1/2):
    lda     #NO_TIMER_COL   ; 2
    ldy     #TIMER_COL      ; 2
    ldx     colorLst_R + NUM_CELLS/2;2
    bne     .timerStdCol    ; 3/2
    lda     roundFlags
    and     #BURN_EMPTY
    beq     .timerStdCol
    lda     #NO_TIMER_CX_COL; 2
    ldy     #TIMER_CX_COL   ; 2
.timerStdCol
    sta     .noTimerCol     ; 3
    sty     .timerCol       ; 3 = 18

    ldx     #%000               ; 2
    stx     PF0
    stx     PF1
    stx     PF2
    stx     NUSIZ1              ; 2
    lda     #%100001            ; 2         quad size ball, reflected PF
    sta     CTRLPF              ; 3
    lda     #$70                ; 2         -7
    sta     HMP1                ; 3
    stx     GRP1                ; 3
    stx     COLUP1
    lda     timerHi             ; 3
    clc
    adc     #6                  ; 2
    sta     WSYNC               ; 3 = 32
;---------------------------------------
    sta     RESP1               ; 3         prepare border sprite for energy bar
    stx     GRP1                ; 3         clear P0 & P1
WaitBar
    sbc     #$0f                ; 2
    bcs     WaitBar             ; 2/3
    CHECKPAGE WaitBar

    tay                         ; 2
    lda     HmTbl,y             ; 4
    sta     HMBL                ; 3
    sta.w   RESBL               ; 4         @23..73!
    sta     WSYNC
;---------------------------------------
    sta     HMOVE               ; 3
    stx     COLUP1              ; 2 =  5    X = 0
    lda     timerHi             ; 3
    lsr                         ; 2
    lsr                         ; 2
    tay                         ; 2
    lda     #$ff                ; 2 = 11
    sta     GRP1                ; 3
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
    lda     TimerF-35,y         ; 4
    bcs     .setPF0b            ; 3 = 56

.below11                        ;   @23
    ldx     TimerF-3,y          ; 4
    stx     .pf1a               ; 3
    bcc     .setPF2a            ; 3

.below19
    ldx     TimerR-11,y         ; 4
    stx     .pf2a
    bcc     .setPF2b

.below27
    ldx     TimerF-19,y         ; 4
    stx     .pf2b
    bcc     .setPF1b

.below35
    ldx     TimerR-27,y         ; 4
    stx     .pf1b               ; 3
    bcc     .setPF0b            ; 3

.below3
    ldx     TimerR+5,y          ; 4
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

    ldx     #BAR_HEIGHT         ; 3
    lda     .noTimerCol         ; 3
    sta     COLUPF              ; 2
.loopBar
;    lda     #NO_TIMER_COL
    sta     WSYNC               ; 3 =  9
;---------------------------------------
;    sta     COLUPF              ; 3
    nop                         ; 2
    lda     .timerCol           ; 3
    sta     COLUBK              ; 3
    lda     #%10                ; 2
    sta     ENABL               ; 3 = 13

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
    ldy     #%10011             ; 2
    sty     HMP1                ; 3
    sty     VDELP0              ; 3
    sty     NUSIZ0              ; 3
    lda     #$0e                ; 2
    sta     COLUP0              ; 3
    SLEEP   8                   ; 8
    sty     VDELP1              ; 3 = 27    @01
    sty     NUSIZ1              ; 3
    sta     COLUP1              ; 3 =  6

    stx     ENABL               ; 3
    stx     COLUBK              ; 3         @10
    stx     COLUPF              ; 3 =  9    @13

    ldx     #DIGIT_BYTES-1      ; 2
    ldy     #DIGIT_BYTES*2-2    ; 2 =  4
.loopMove
    lda     .digitPtrLst,x      ; 4
    sta     .digitPtrLst,y      ; 5
    dey                         ; 2
    dey                         ; 2
    dex                         ; 2
    bne     .loopMove           ; 3/2=18/17 (last byte already at right position)
; total: 4+5*18-1=93                        @13+93 = @30

    lda     #>DigitGfx          ; 2

    sta     RESP0               ; 3         @38!
    sta     RESP1               ; 3  = 6    @41!

    sta     .digitPtr0+1        ; 3
    sta     .digitPtr1+1        ; 3
    sta     .digitPtr2+1        ; 3
    sta     .digitPtr3+1        ; 3
    sta     .digitPtr4+1        ; 3
    sta     .digitPtr5+1        ; 3 = 18    @59


;    lda     #<One
;    sta     .digitPtr0
;    lda     #<Two
;    sta     .digitPtr1
;    lda     #<Three
;    sta     .digitPtr2
;    lda     #<Four
;    sta     .digitPtr3
;    lda     #<Five
;    sta     .digitPtr4
;    lda     #<Zero
;    sta     .digitPtr5

    ldy     #FONT_H-1       ; 2
.loopScore
    lda     (.digitPtr0),y  ; 5
    sta     WSYNC           ; 3 =  8
;---------------------------------------
    sta     HMOVE           ; 3
    sta     GRP0            ; 3
    lda     (.digitPtr1),y  ; 5
    sta     GRP1            ; 3
    lda     (.digitPtr2),y  ; 5
    sta     GRP0            ; 3 = 22
    lax     (.digitPtr5),y  ; 5
    txs                     ; 2
    lax     (.digitPtr3),y  ; 5
    lda     (.digitPtr4),y  ; 5
    stx     GRP1            ; 3 = 20
    sta     GRP0            ; 3
    tsx                     ; 2
    stx     GRP1            ; 3
    sta     GRP0            ; 3
    sta     HMCLR           ; 3
    dey                     ; 2
    bpl     .loopScore      ; 3/2=19
    iny
    sty     GRP0
    sty     GRP1
    sty     GRP0
    sty     VDELP0
    sty     VDELP1

    ldx     #2
.waitScreen
    lda     INTIM
    bne     .waitScreen
    sta     WSYNC
;---------------------------------------
    stx     VBLANK
    ldx     #$ff
    txs
    jmp     ContKernel
; /DrawKernel

KernelCode ; patched into
    KERNEL_CODE

    ALIGN_FREE_LBL   256, "PatchTbl"

PatchTbl
    .byte   Col0 + 1 - PD
    .byte   Col1 + 1 - PD
    .byte   Col2 + 1 - PD
    .byte   Col3 + 1 - PD
    .byte   Col4 + 1 - PD
    .byte   .tmpColP0       ; -> COLUP0
    .byte   Col6 + 1 - PD
    .byte   Col7 + 1 - PD
    .byte   Col8 + 1 - PD
    .byte   .tmpColP1       ; -> COLUP1
    .byte   Col10 + 1 - PD
    .byte   Col11 + 1 - PD
    .byte   Col12 + 1 - PD
    CHECKPAGE_DATA PatchTbl
NUM_COLS    = . - PatchTbl ; 13


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
.waitTim
    lda     INTIM
    bne     .waitTim

    lda     #%1110          ; each '1' bits generate a VSYNC ON line (bits 1..3)
.loopVSync
    sta     WSYNC           ; 1st '0' bit resets Vsync, 2nd '0' bit exits loop
    sta     VSYNC
    lsr
    bne     .loopVSync      ; branch until VSYNC has been reset

    inc     frameCnt

  IF NTSC_TIM
    lda     #44-4+4         ; 38*64 = 2432
  ELSE
    lda     #77
  ENDIF
    sta     TIM64T

    lda     SWCHB
    lsr
    bcs     .skipReset
    jmp     Start

.skipReset
.tmpSwchA   = tmpVars

    lda     gameState                   ; GAME_RUNNING?
    bmi     .checkInput
    bit     INPT4
    bmi     .contStopped
    ora     #GAME_RUNNING
    sta     gameState
.contStopped
    jmp     .skipRunning

.checkInput
TIM_S
    lax     SWCHA
    bit     SWCHB
    bvs     .normalDirs
; reverse directions
    lsr
    and     #%01011111
    sta     .tmpSwchA
    txa
    sec
    rol
    and     #%10101111
    ora     .tmpSwchA
    tax
.normalDirs
    cpx     #$ff
    lda     moveSum
    bcc     .dirPressed
    lda     #MOVE_SPEED-1       ; reset move delay
.dirPressed
    sbc     #MOVE_SPEED-1
    sta     moveSum
    bcs     .skipDirs
    bit     $ffff               ; set V-flag
    jsr     ScrollCells
.skipDirs

.skipRunning

    bit     INPT5
    bmi     .skipResetColors
    lda     frameCnt
    and     #$1f
    bne     .skipResetColors
    jsr     InitColors
.skipResetColors
; /VerticalBlank

    jmp     DrawKernel
ContKernel

;---------------------------------------------------------------
OverScan SUBROUTINE
;---------------------------------------------------------------
  IF NTSC_TIM
    lda     #36-7+6+1
  ELSE
    lda     #63
  ENDIF
    sta     TIM64T

TIM_OVS
    lda     gameState                   ; GAME_RUNNING?
    bmi     .contRunning
    jmp     .skipRunning

.contRunning
    lda     #TIMER_SPEED
    bit     roundFlags                  ; BURN_EMPTY?
    bvc     .skipBurn
    ldx     colorLst_R+NUM_CELLS/2
    bne     .skipBurn
    stx     AUDF1
    lda     #$08
    sta     AUDC1
;    lda     #10
    sta     soundIdx1
    lda     #TIMER_SPEED * 8
.skipBurn
    clc
    adc     timerLo
    sta     timerLo
    bcc     .skipTimerHi
    lda     timerHi
    beq     .skipTimerHi
    dec     timerHi
.skipTimerHi

; check for color match (TODO: can be done every 2nd frame)
    bit     SWCHB
    bpl     .coarseCheck
    ldx     #NUM_CELLS/2
    lda     colorLst_R+NUM_CELLS/2
    cmp     targetCol
    beq     .foundTargetCol
    jmp     .skipNewCol

.coarseCheck
; check if any cell nearby is close (delta hue OR (todo) delta val <= 2)
;.tmpDiff    = tmpVars
MAX_HUE_DIFF    = $01
MAX_VAL_DIFF    = $02

; compare hue:
.checkHue
    lda     targetCol
    lsr
    lsr
    lsr
    lsr
    tay
    lda     colorLst_R+NUM_CELLS/2
    lsr
    lsr
    lsr
    lsr
    tax
    lda     HueIdx,y
    sec
    sbc     HueIdx,x                    ; same hue?
    beq     .checkRangeValue            ;  yes, check value +/-2
    adc     #MAX_HUE_DIFF               ; -2 -> -1x; -1 -> 0; 0 -> 2; 1 -> 3; 2 -> 4x
    cmp     #MAX_HUE_DIFF*2+1+1         ; difference <= 1?
    bcc     .checkSameValue             ;  yes, check for same value
    bcs     .skipNewCol                 ;  no, no match

; compare value:
.checkRangeValue    ; hue is the same
    lda     targetCol
    sec
    sbc     colorLst_R+NUM_CELLS/2
    clc
    adc     #MAX_VAL_DIFF
    cmp     #MAX_VAL_DIFF*2+1
    bcs     .skipNewCol                 ;  no, no match
    bcc     .foundTargetCol             ;  yes, match!

.checkSameValue
    lda     targetCol
    eor     colorLst_R+NUM_CELLS/2
    and     #$0f
    bne     .skipNewCol                 ;  no, no match
.foundTargetCol
    lda     roundFlags
    and     #KEEP_CELLS
    bne     .skipEmpty
    lda     #EMPTY_COL
    sta     colorLst_W+NUM_CELLS/2
.skipEmpty
    jsr     GetRandomCellIdx
    sta     targetCol
; increase score:
    lda     #$50
    jsr     AddScore0
; start found soundIdx0:
    lda     #FOUND_SOUND_LEN
    sta     soundIdx0
    lda     #$04
    sta     AUDC0
    lda     #$0e
    sta     AUDF0
; add extra time:
    lda     #CELL_BONUS
    jsr     AddTimer
    dec     cellCnt
    bne     .skipNextRound
    jsr     NextRound
.skipNextRound

.skipNewCol

    lda     frameCnt                ; TODO: can be done every 2nd frame
    and     #$03
    beq     .doApplyObst
.skipApplyObstJmp
    jmp     .skipApplyObst

.doApplyObst
    lda     obstSum
    clc
    adc     #OBST_SPPED
    sta     obstSum
    bcc     .skipApplyObstJmp
;    lda     frameCnt
;    and     #$3f
;    bne     .skipApplyObst
; check for scrolling
    lda     roundFlags
    and     #SWAP_FOUND|SCROLL_ROWS|SCROLL_COLS
    beq     .skipApplyObst
    ldx     soundIdx1
    bne     .skipTick
    ldx     #TICK_SOUND_LEN
    stx     soundIdx1
    ldx     #$08
    stx     AUDC1
    ldx     #$04
    stx     AUDV1
.skipTick
    and     #SCROLL_ROWS|SCROLL_COLS
    beq     .skipScrolls
; scroll either rows or columns or both
; determine direction:
    tax
    jsr     NextRandom
    and     ScrollMask,x        ; 0..1, 2..3, 0..3; %01, %10, %11
    ora     DirOfs,x            ; 0, 2, 0
    tay
    lda     DirBits,y
    asl                         ; A = ????....
    tax
    bcs     .scrollCol
; determine row:
    ldy     #2                  ; 2 tries, 9/16 ok, 7/16*9/16 ok
.randomRow
    jsr     NextRandom          ; TODO: improve
    and     #$0f
    cmp     #NUM_ROWS
    bcc     .validRow
    dey
    bne     .randomRow
    lsr
.validRow
    tay

;    ldy     #0
;    ldx     #%01111110

    lda     RowOfs,y
    cpx     #%01111110
    bne     .scrollLeft
    adc     #NUM_COLS-1
.scrollLeft
;    lda     RowOfsR,y
;    cpx     #%01111110
;    beq     .scrollRight
;    lda     RowOfsL,y
;.scrollRight
    tay
    bpl     .doScroll
    DEBUG_BRK

.scrollCol
.randomCol
    jsr     NextRandom          ; TODO: improve
    and     #$0f
    cmp     #NUM_COLS
    bcs     .randomCol
    tay
    lda     ColOfsU,y
    cpx     #%11101110
    beq     .scrollUp
    lda     ColOfsD,y
.scrollUp
    tay
.doScroll
    clv
    jsr     ScrollCells
.skipScrolls
; swap cells:
    START_TMP
.xCell0     ds 1
    END_TMP
    lda     roundFlags
    and     #SWAP_FOUND
    beq     .skipSwap
    jsr     GetRandomCellIdx
    stx     .xCell0
    jsr     GetRandomCellIdx
    ldy     .xCell0
    lda     colorLst_R,x
    pha
    lda     colorLst_R,y
    sta     colorLst_W,x
    pla
    sta     colorLst_W,y
.skipSwap
.skipApplyObst

.skipRunning

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
    ldy     #8
    dec     soundIdx1
.skipSound1
    sty     AUDV1

TIM_OVE ; 1870
    jmp     MainLoop

;---------------------------------------------------------------
ScrollCells SUBROUTINE
;---------------------------------------------------------------
    START_TMP
.yEnd       .byte
    END_TMP

    txa
    bpl     .doRight
    jmp     .skipRight

;---------------------------------------------------------------
.doRight
    asl
    asl
    bpl     .downR
    asl
    bpl     .upR
; shift colors right:
    bvc     .singleScrollR
  IF BLOCK_CELLS
    lda     roundFlags                  ;           BLOCK_FLAG?
    bpl     .skipBlockR
    lda     colorLst_R+NUM_CELLS/2-1    ;           EMPTY_COL?
    beq     .skipDirsJmp
.skipBlockR
  ENDIF
    ldy     #NUM_CELLS
    lda     #0
    beq     .wholeScrollR

.singleScrollR
    tya
    sec
    sbc     #NUM_COLS
.wholeScrollR
    sta     .yEnd
.loopRowsR
    lda     colorLst_R-1,y
    pha
    dey
    ldx     #NUM_COLS-2
.loopColsR
    lda     colorLst_R-1,y              ; 4
    sta     colorLst_W,y                ; 5
    dey                                 ; 2
    dex                                 ; 2
    bpl     .loopColsR                  ; 3/2=16/15
    pla
    sta     colorLst_W,y
    tya
    cpy     .yEnd
    bne     .loopRowsR
TIM_R
; 2005 cycles
    nop
.skipDirsJmp
    jmp     .skipDirs

;---------------------------------------------------------------
.downR
; move cells down right:
  IF BLOCK_CELLS
    lda     roundFlags                  ;           BLOCK_FLAG?
    bpl     .skipBlockDR
    lda     colorLst_R+NUM_CELLS/2+NUM_COLS-1   ;   EMPTY_COL?
    beq     .skipDirsJmp
.skipBlockDR
  ENDIF
    jsr     SaveRightColumn
    ldy     #NUM_COLS-2
.loopColsDR
    clc
    ldx     colorLst_R,y
.loopRowsDR
    lda     colorLst_R+NUM_COLS*1,y     ; 4
    sta     colorLst_W+NUM_COLS*0+1,y   ; 5
    lda     colorLst_R+NUM_COLS*2,y     ; 4
    sta     colorLst_W+NUM_COLS*1+1,y   ; 5
    tya                                 ; 2
    adc     #NUM_COLS*2                 ; 2
    tay                                 ; 2
    cpy     #NUM_CELLS-NUM_COLS         ; 2
    bcc     .loopRowsDR                 ; 3/2=29/28
    txa
    sta     colorLst_W+1,y
    tya
    sbc     #NUM_CELLS-NUM_COLS+1
    tay
    bcs     .loopColsDR
    ldy     #NUM_CELLS-NUM_COLS*2
TIM_DR
; 1913 cycles
    jmp     .loadColumnDown             ;175

;---------------------------------------------------------------
.upR
; move cells up right:
  IF BLOCK_CELLS
    lda     roundFlags
    bpl     .skipBlockUR
    lda     colorLst_R+NUM_CELLS/2-NUM_COLS-1   ;   EMPTY_COL?
    beq     .skipDirsJmp
.skipBlockUR
  ENDIF
    jsr     SaveRightColumn
    ldy     #NUM_CELLS-NUM_COLS-2
.loopColsUR
    ldx     colorLst_R+NUM_COLS,y
    sec
.loopRowsUR
    lda     colorLst_R-NUM_COLS*0,y     ; 4
    sta     colorLst_W+NUM_COLS*1+1,y   ; 5
    lda     colorLst_R-NUM_COLS*1,y     ; 4
    sta     colorLst_W-NUM_COLS*0+1,y   ; 5
    tya                                 ; 2
    sbc     #NUM_COLS*2                 ; 2
    tay                                 ; 2
    bcs     .loopRowsUR                 ; 3/2=27/26
    txa
    sta     colorLst_W-256+NUM_COLS+1,y
    tya
    adc     #NUM_CELLS-NUM_COLS-1
    tay
    cpy     #NUM_CELLS-NUM_COLS*2
    bcs     .loopColsUR
; load old right into left column:
    ldy     #NUM_CELLS-NUM_COLS
TIM_UR
; 1845 cycles
    jmp     .loadColumnUp

;---------------------------------------------------------------
.skipRight
    asl
    bpl     .doLefts
    jmp     .skipLeft

.doLefts
    asl
    bpl     .downL
    asl
    bmi     .doLeft
    jmp     .upL

.doLeft
; move cells left:
    bvc     .singleScrollL
  IF BLOCK_CELLS
    lda     roundFlags
    bpl     .skipBlockL
    lda     colorLst_R+NUM_CELLS/2+1    ;           EMPTY_COL?
    beq     .skipDirsJmp2
.skipBlockL
  ENDIF
    ldy     #0
    lda     #NUM_CELLS
    bne     .wholeScrollL

.singleScrollL
    tya
    clc
    adc     #NUM_COLS
.wholeScrollL
    sta     .yEnd
.loopRowsL
    lda     colorLst_R,y
    pha
    iny
    ldx     #NUM_COLS-2
.loopColsL
    lda     colorLst_R,y                ; 4
    sta     colorLst_W-1,y              ; 5
    iny                                 ; 2
    dex                                 ; 2
    bpl     .loopColsL                  ; 3/2=16/15
    pla
    sta     colorLst_W-1,y
    cpy     .yEnd                       ; #NUM_CELLS
    bcc     .loopRowsL
TIM_L
; 2117 cycles (.loopColsL crosses page)
    bcs     .skipDirsJmp2

.downL
; move cells down left:
  IF BLOCK_CELLS
    lda     roundFlags                  ;           BLOCK_FLAG?
    bpl     .skipBlockDL
    lda     colorLst_R+NUM_CELLS/2+NUM_COLS+1   ;   EMPTY_COL?
    beq     .skipDirsJmp2
.skipBlockDL
  ENDIF
    jsr     SaveLeftColumn
    ldy     #0
    clc
.loopColsDL
    ldx     colorLst_R+1,y
.loopRowsDL
    lda     colorLst_R+NUM_COLS*1+1,y   ; 4
    sta     colorLst_W+NUM_COLS*0,y     ; 5
    lda     colorLst_R+NUM_COLS*2+1,y   ; 4
    sta     colorLst_W+NUM_COLS*1,y     ; 5
    tya                                 ; 2
    adc     #NUM_COLS*2                 ; 2
    tay                                 ; 2
    cpy     #NUM_CELLS-NUM_COLS         ; 2
    bcc     .loopRowsDL                 ; 3/2=29/28 -> 14,5
    sbc     #NUM_CELLS-NUM_COLS-1
    tay
    txa
    sta     colorLst_W+NUM_CELLS-NUM_COLS-1,y
    cpy     #NUM_COLS-1
    bcc     .loopColsDL
    ldy     #NUM_CELLS-NUM_COLS-1
TIM_DL
; 1893 cycles
    nop
.loadColumnDown
TIM_SLD
    lda     colorBuf_R              ; saved bottom cell
    sta     colorLst_W+NUM_COLS,y   ; ...to top cell
    ldx     #NUM_ROWS-1
    sec
.loopLoadDown
    lda     colorBuf_R,x
    sta     colorLst_W,y
    tya
    sbc     #NUM_COLS
    tay
    dex
    bne     .loopLoadDown
TIM_LD
; 172 cycles
    nop
.skipDirsJmp2
    jmp     .skipDirs

;---------------------------------------------------------------
.upL
; move cells up left:
  IF BLOCK_CELLS
    lda     roundFlags                  ;           BLOCK_FLAG?
    bpl     .skipBlockUL
    lda     colorLst_R+NUM_CELLS/2-NUM_COLS+1   ;   EMPTY_COL?
    beq     .skipDirsJmp2
.skipBlockUL
  ENDIF
    jsr     SaveLeftColumn
    ldy     #NUM_CELLS-NUM_COLS*2+1
.loopColsUL
    ldx     colorLst_R+NUM_COLS,y
    sec
.loopRowsUL
    lda     colorLst_R-NUM_COLS*0,y     ; 4
    sta     colorLst_W+NUM_COLS*1-1,y   ; 5
    lda     colorLst_R-NUM_COLS*1,y     ; 4
    sta     colorLst_W-NUM_COLS*0-1,y   ; 5
    tya                                 ; 2
    sbc     #NUM_COLS*2                 ; 2
    tay                                 ; 2
    bcs     .loopRowsUL                 ; 3/2=27/26
    txa
    sta     colorLst_W-256+NUM_COLS-1,y
    tya
    adc     #NUM_CELLS-NUM_COLS+1
    tay
    cpy     #NUM_CELLS-NUM_COLS
    bcc     .loopColsUL
; load old left into right column:
    ldy     #NUM_CELLS-1
TIM_UL
; 1847 cycles
    nop
TIM_SLU
.loadColumnUp
    ldx     #NUM_ROWS-2             ; 2         = 7
    sec                             ; 2 =  4
.loopLoadUp
    lda     colorBuf_R,x            ; 4
    sta     colorLst_W,y            ; 5
    tya                             ; 2
    sbc     #NUM_COLS               ; 2
    tay                             ; 2
    dex                             ; 2
    bpl     .loopLoadUp             ; 3 = 20/19
    lda     colorBuf_R+NUM_ROWS-1   ; 4         saved top cell
    sta     colorLst_W,y            ; 5         ...to bottom cell
TIM_LU
; 172 cycles (1x unrolling saves 42 cycles)
    bcs     .skipDirsJmp2
    DEBUG_BRK

;---------------------------------------------------------------
.skipLeft
; check for down:
    asl
    bmi     .skipDown
; move cells down:
    bvc     .singleScrollD
  IF BLOCK_CELLS
    lda     roundFlags                  ;           BLOCK_FLAG?
    bpl     .skipBlockD
    lda     colorLst_R+NUM_CELLS/2+NUM_COLS     ;   EMPTY_COL?
    beq     .skipDirs
.skipBlockD
  ENDIF
    ldy     #NUM_COLS-1
    lda     #-1
    bne     .wholeScrollD

.singleScrollD
    tya
    sec
    sbc     #1
.wholeScrollD
    sta     .yEnd
.loopColsD
    clc
    ldx     colorLst_R,y
.loopRowsD
    lda     colorLst_R+NUM_COLS*1,y     ; 4
    sta     colorLst_W+NUM_COLS*0,y     ; 5
    lda     colorLst_R+NUM_COLS*2,y     ; 4
    sta     colorLst_W+NUM_COLS*1,y     ; 5
    tya                                 ; 2
    adc     #NUM_COLS*2                 ; 2
    tay                                 ; 2
    cpy     #NUM_CELLS-NUM_COLS         ; 2
    bcc     .loopRowsD                  ; 3/2=29/28
    txa
    sta     colorLst_W,y
    tya
    sbc     #NUM_CELLS-NUM_COLS+1
    tay
;    txa
;    sta     colorLst_W+NUM_CELLS-NUM_COLS+1,y      ; wraps and causes RW-Trap!
    cpy     .yEnd
    bne     .loopColsD
TIM_D
; 1845 cycles
    beq     .skipDirs
; (2534, 2222, 1897) 1819 cycles = ~23.9 scanlines

;---------------------------------------------------------------
.skipDown
    asl
    bmi     .skipUp
; move cells up:
    bvc     .singleScrollU
  IF BLOCK_CELLS
    lda     roundFlags                  ;           BLOCK_FLAG?
    bpl     .skipBlockLU
    lda     colorLst_R+NUM_CELLS/2-NUM_COLS     ;   EMPTY_COL?
    beq     .skipDirs
.skipBlockLU
  ENDIF
    ldy     #NUM_CELLS-1
    lda     #NUM_CELLS-NUM_COLS-1
    bne     .wholeScrollU

.singleScrollU
    tya
    sec
    sbc     #1
.wholeScrollU
    sta     .yEnd
    sec
.loopColsU
    ldx     colorLst_R,y
.loopRowsU
    lda     colorLst_R-NUM_COLS*1,y     ; 4
    sta     colorLst_W-NUM_COLS*0,y     ; 5
    lda     colorLst_R-NUM_COLS*2,y     ; 4
    sta     colorLst_W-NUM_COLS*1,y     ; 5
    tya                                 ; 2
    sbc     #NUM_COLS*2                 ; 2
    tay                                 ; 2
    cpy     #NUM_COLS*2-1               ; 2
;    bpl     .loopRowsU                  ; 3/2=29/28
    bcs     .loopRowsU                  ; 3/2=29/28
;    clc
    adc     #NUM_CELLS-NUM_COLS-1
    tay
    txa
    sta     colorLst_W-NUM_CELLS+NUM_COLS+1,y
    cpy     .yEnd                        ; #NUM_CELLS-NUM_COLS
    bne     .loopColsU
TIM_U
; 1826 cycles
    nop
;    jmp     .skipDirs
.skipUp
.skipDirs
    rts

;---------------------------------------------------------------
SaveRightColumn SUBROUTINE
;---------------------------------------------------------------
    ldy     #NUM_CELLS-1
    NOP_W
SaveLeftColumn
    ldy     #NUM_CELLS-NUM_COLS
    ldx     #NUM_ROWS-1
    sec
.loopSave
    lda     colorLst_R,y
    sta     colorBuf_W,x
    tya
    sbc     #NUM_COLS
    tay
    dex
    bpl     .loopSave
    rts

ScrollMask = . - 1
    .byte   %01, %01, %11
DirOfs = . - 1
    .byte   %10, 0, 0
DirBits
    .byte   %11110111           ; column up
    .byte   %11101111           ; column down
    .byte   %01011111           ; row left
    .byte   %00111111           ; row right
RowOfs
_VAL SET 0
  REPEAT NUM_ROWS
    .byte   _VAL
_VAL SET _VAL + NUM_COLS
  REPEND
ColOfsD
_VAL SET 0
  REPEAT NUM_COLS
    .byte   _VAL
_VAL SET _VAL + 1
  REPEND
ColOfsU
_VAL SET NUM_CELLS-NUM_COLS
  REPEAT NUM_COLS
    .byte   _VAL
_VAL SET _VAL + 1
  REPEND

;---------------------------------------------------------------
GetRandomCellIdx SUBROUTINE
;---------------------------------------------------------------
; uses:    A, X
; returns: A = color, X = index
.repeatRandom
    ldx     #INDEX_LEN-1
.loopRandom
    jsr     NextRandom
    and     #$7f
    cmp     #NUM_CELLS
    bcc     .cellOk
    lsr                     ; TODO: improve
.cellOk
    cmp     IndexTbl,x
    beq     .loopRandom
    dex
    bne     .loopRandom
    tax
    lda     colorLst_R,x
    cmp     #EMPTY_COL
    beq     .repeatRandom
    rts

IndexTbl
    .byte   NUM_CELLS/2-NUM_COLS-1
    .byte   NUM_CELLS/2-NUM_COLS
    .byte   NUM_CELLS/2-NUM_COLS+1
    .byte   NUM_CELLS/2-1
    .byte   NUM_CELLS/2
    .byte   NUM_CELLS/2+1
    .byte   NUM_CELLS/2+NUM_COLS-1
    .byte   NUM_CELLS/2+NUM_COLS
    .byte   NUM_CELLS/2+NUM_COLS+1
INDEX_LEN = . - IndexTbl

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

;---------------------------------------------------------------
GameInit SUBROUTINE
;---------------------------------------------------------------
    ldx     #KernelCodeEnd-KernelCode-1
.loopCopy
    lda     KernelCode,x
    sta     RAMKernel,x
    dex
    bpl     .loopCopy

    lda     INTIM
    sta     randomLo

    lda     #START_ROUND
    sta     round

    lda     #MAX_TIMER-ROUND_BONUS
    sta     timerHi
    bne     .contInitGame

NextRound
    ldx     round
    inx
    cpx     #NUM_ROUNDS
    bcc     .roundOk
    ldx     #NUM_ROUNDS-4
.roundOk
    stx     round
.contInitGame
    lda     #ROUND_BONUS
    jsr     AddTimer
    ldx     #255
    stx     timerLo
    inx
    sta     frameCnt
    asl     gameState           ; remove GAME_RUNNING flag
    lsr     gameState

    lda     #$04
    sta     AUDC0
    lda     #$0a
    sta     AUDF0
    lda     #BONUS_SOUND_LEN
    sta     soundIdx0

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
    lda     MultTbl,y
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
;    ldx     MultTbl,y
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

    ldy     round
    lda     RoundFlags,y
    sta     roundFlags
    lda     RoundLen,y
;    tya
;    clc
;    adc     #ROUND_CELLS
    sta     cellCnt

    jsr     GetRandomCellIdx
    sta     targetCol
    rts
; /GameInit

;---------------------------------------------------------------
AddTimer SUBROUTINE
;---------------------------------------------------------------
    clc
    adc     timerHi
    cmp     #MAX_TIMER
    bcc     .setTimerHi
    sbc     #MAX_TIMER
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
    adc     BcdTab,y        ; 4 = 22    @26
    jsr     AddScore0
    lda     #MAX_TIMER
.setTimerHi
    sta     timerHi
    rts

;---------------------------------------------------------------
AddScore0 SUBROUTINE
;---------------------------------------------------------------
    ldy     #0
AddScore
    sed
    clc
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

;    ALIGN_FREE_LBL  256, "SetXPos"
;
;;---------------------------------------------------------------
;SetXPos SUBROUTINE
;;---------------------------------------------------------------
;    sec
;    sta     WSYNC
;WaitObject:
;    sbc     #$0f            ; 2
;    bcs     WaitObject      ; 3/2
;    CHECKPAGE WaitObject
;    eor     #$07            ; 2
;    asl                     ; 2
;    asl                     ; 2
;    asl                     ; 2
;    asl                     ; 2
;    sta     HMP0,x          ; 4
;    sta.wx  RESP0,x         ; 5     @23!
;    rts
;; SetXPos


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
    ds  FONT_H, %11111100
Blank
    ds  FONT_H, 0
  CHECKPAGE_DATA_LBL DigitGfx, "DigitGfx"

DigitPtrTbl
    .byte   <Zero,  <One,   <Two,   <Three, <Four
    .byte   <Five,  <Six,   <Seven, <Eight, <Nine
ID_BLANK = . - DigitPtrTbl
    .byte   <Blank

MultTbl
_IDX    SET 0
    REPEAT NUM_ROWS+1
    .byte  _IDX * NUM_COLS
_IDX    SET _IDX + 1
    REPEND

    .byte   "JTZ"

  IF NTSC_COL
ColorTbl
;    .byte   BROWN           ; $20
;    .byte   ORANGE          ; $30
;    .byte   RED             ; $40
;    .byte   MAUVE           ; $50
;    .byte   VIOLET          ; $60
;    .byte   PURPLE          ; $70
;    .byte   BLUE            ; $80
;    .byte   BLUE_CYAN       ; $90
;    .byte   CYAN            ; $a0
;    .byte   CYAN_GREEN      ; $b0
;;    .byte   GREEN           ; $c0
;    .byte   GREEN_YELLOW    ; $d0
;    .byte   GREEN_BEIGE     ; $e0
;;    .byte   BEIGE           ; $f0
;    .byte   YELLOW          ; $10

    .byte   YELLOW          ; $10
    .byte   BROWN           ; $20
;    .byte   ORANGE          ; $30
    .byte   RED             ; $40
    .byte   MAUVE           ; $50
    .byte   VIOLET          ; $60
    .byte   PURPLE          ; $70
    .byte   BLUE            ; $80
    .byte   BLUE_CYAN       ; $90
    .byte   CYAN            ; $a0
    .byte   CYAN_GREEN      ; $b0
    .byte   GREEN           ; $c0
    .byte   GREEN_YELLOW    ; $d0
    .byte   GREEN_BEIGE     ; $e0
;    .byte   BEIGE           ; $f0
;    .byte   YELLOW          ; $10
NUM_COLS    = . - ColorTbl  ; 13
HueIdx = . - 1
    .byte   0       ; YELLOW          $10
    .byte   1       ; BROWN           $20
                    ; (ORANGE)        $30
    .byte   2       ; RED             $40
    .byte   3       ; MAUVE           $50
    .byte   4       ; VIOLET          $60
    .byte   5       ; PURPLE          $70
    .byte   6       ; BLUE            $80
    .byte   7       ; BLUE_CYAN       $90
    .byte   8       ; CYAN            $a0
    .byte   9       ; CYAN_GREEN      $b0
    .byte   10      ; GREEN           $c0
    .byte   11      ; GREEN_YELLOW    $d0
    .byte   12      ; GREEN_BEIGE     $e0
                    ; (BEIGE)         $f0
  ELSE
PAL_WHITE   = BLACK+$10     ; $10
ColorTbl
    .byte   YELLOW          ; $20
    .byte   ORANGE          ; $40
    .byte   RED             ; $60
    .byte   MAUVE           ; $80
    .byte   VIOLET          ; $a0
    .byte   PURPLE          ; $c0
    .byte   BLUE            ; $d0
    .byte   BLUE_CYAN       ; $b0
    .byte   CYAN            ; $90
    .byte   CYAN_GREEN      ; $70
    .byte   GREEN           ; $50
    .byte   GREEN_YELLOW    ; $30
    .byte   PAL_WHITE       ; $10
NUM_COLS    = . - ColorTbl  ; 13
HueIdx = . - 1
    .byte   12      ; PAL_WHITE       $10
    .byte   0       ; YELLOW          $20
    .byte   11      ; GREEN_YELLOW    $30
    .byte   1       ; ORANGE          $40
    .byte   10      ; GREEN           $50
    .byte   2       ; RED             $60
    .byte   9       ; CYAN_GREEN      $70
    .byte   3       ; MAUVE           $80
    .byte   8       ; CYAN            $90
    .byte   4       ; VIOLET          $a0
    .byte   7       ; BLUE_CYAN       $b0
    .byte   5       ; PURPLE          $c0
    .byte   6       ; BLUE            $d0
  ENDIF

LumTbl
  IF 0
;    .byte   $04
;    .byte   $06
;    .byte   $08
;    .byte   $0a
;    .byte   $0e
;    .byte   $0a
;    .byte   $06
;    .byte   $04
;    .byte   $02
  ENDIF
  IF 1
    .byte   $00
    .byte   $02
    .byte   $04
    .byte   $06
    .byte   $08
    .byte   $0a
    .byte   $0c
    .byte   $0e
    .byte   $0e
  ENDIF

TimerR
    .byte   %11111110
    .byte   %11111100
    .byte   %11111000
    .byte   %11110000
    .byte   %11100000
    .byte   %11000000
    .byte   %10000000
    .byte   %00000000
TimerF
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

BcdTab  ; 5 entries work up to 79 (79 / 16 = 4.93)
    .byte $00, $06, $12, $18, $24;, $30, $36

  IF 0 ;{
RoundFlags
    .byte   0
    .byte   KEEP_CELLS
    .byte               SWAP_FOUND
    .byte   KEEP_CELLS |SWAP_FOUND

    .byte                          SCROLL_COLS
    .byte   KEEP_CELLS |           SCROLL_COLS
    .byte               SWAP_FOUND|SCROLL_COLS
    .byte   KEEP_CELLS |SWAP_FOUND|SCROLL_COLS

    .byte                                      SCROLL_ROWS
    .byte   KEEP_CELLS |                       SCROLL_ROWS
    .byte               SWAP_FOUND|            SCROLL_ROWS
    .byte   KEEP_CELLS |SWAP_FOUND|            SCROLL_ROWS

    .byte                          SCROLL_COLS|SCROLL_ROWS
    .byte   KEEP_CELLS |           SCROLL_COLS|SCROLL_ROWS
    .byte               SWAP_FOUND|SCROLL_COLS|SCROLL_ROWS
    .byte   KEEP_CELLS |SWAP_FOUND|SCROLL_COLS|SCROLL_ROWS

    .byte   BLOCK_EMPTY
    .byte   BLOCK_EMPTY|SWAP_FOUND
    .byte   BLOCK_EMPTY|           SCROLL_COLS
    .byte   BLOCK_EMPTY|SWAP_FOUND|SCROLL_COLS
    .byte   BLOCK_EMPTY|                       SCROLL_ROWS
    .byte   BLOCK_EMPTY|SWAP_FOUND|            SCROLL_ROWS
    .byte   BLOCK_EMPTY|           SCROLL_COLS|SCROLL_ROWS
    .byte   BLOCK_EMPTY|SWAP_FOUND|SCROLL_COLS|SCROLL_ROWS
NUM_ROUNDS = . - RoundFlags
  ENDIF ;}
  IF 1 ; {
RoundFlags
    .byte   0                                               ; 0
    .byte   KEEP_CELLS

    .byte               SWAP_FOUND
    .byte   KEEP_CELLS |SWAP_FOUND

    .byte   KEEP_CELLS |           SCROLL_COLS              ; 4
    .byte   KEEP_CELLS |SWAP_FOUND|SCROLL_COLS

    .byte   KEEP_CELLS |                       SCROLL_ROWS
    .byte   KEEP_CELLS |SWAP_FOUND|            SCROLL_ROWS

    .byte   KEEP_CELLS |           SCROLL_COLS|SCROLL_ROWS  ; 8
    .byte   KEEP_CELLS |SWAP_FOUND|SCROLL_COLS|SCROLL_ROWS

    .byte   BLOCK_EMPTY                                     ;10
    .byte   BLOCK_EMPTY|SWAP_FOUND|SCROLL_COLS|SCROLL_ROWS  ;11
    .byte   BURN_EMPTY                                      ;12
    .byte   BURN_EMPTY |SWAP_FOUND|SCROLL_COLS|SCROLL_ROWS  ;13
NUM_ROUNDS = . - RoundFlags
RoundLen
    .byte   20, 25
    .byte   30, 30
    .byte   30, 30
    .byte   30, 30
    .byte   35, 40
    .byte   45, 50
    .byte   55, 60
NUM_ROUNDS = . - RoundLen
   ENDIF ;}

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
    .word   Start
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
    .word   Start1
  ENDIF


;===============================================================================
; O U T P U T
;===============================================================================

    LIST OFF
    ECHO ""
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

