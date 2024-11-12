; Color-Match
; (C) 2024 Thomas Jentzsch

; Ideas:
; - 4 game variations:
;   - ordered colors
;   - random row order
;   - random column order
;   - random cells
; - game play:
;   ? remove found cells, either block or fall into
;   ? swap found cells
; - timer, bonus when found fast
;   - either time per block (getting less)
;   - or total time
; - better randomization
; - reset switch


;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

VERSION         = $0002
BASE_ADR        = $f000

  IFNCONST TV_MODE ; manually defined here
NTSC            = 0
PAL60           = 1
PAL50           = 0
  ENDIF

F8SC            = 1     ; create F8SC instead of 4KSC (for Harmony)

ILLEGAL         = 1
DEBUG           = 1


REMOVE_CELLS    = 0
BLOCK_CELLS     = 0     ; TODO
SWAP_CELLS      = 1


SAVEKEY         = 0 ; (-~220) support high scores on SaveKey
PLUSROM         = 0 ; (-~50)

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

; TODO

EMPTY_COL       = BLACK|$1


;===============================================================================
; G A M E - C O N S T A N T S
;===============================================================================

EOR_RND_LO      = $bd           ; %10110100 ($9c, $b4, $bd, $ca, $eb, $fc)

NUM_ROWS        = 9
NUM_COLS        = 13
NUM_CELLS       = NUM_ROWS * NUM_COLS ; = 117

MOVE_SPEED      = $20

STACK_SIZE      = 4


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

frameCnt        .byte
tmpVars         ds 10
randomLo        .byte
randomHi        .byte
targetCol       .byte
moveSum         .byte
sound           .byte

RAMKernel       ds KernelCodeEnd - KernelCode ; (48 bytes reserved for RAM kernel)

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


  MAC KERNEL_CODE
.loopKernel                     ;           @03
Col0
    lda     #$00                ; 2                 #0r
    sta     COLUPF              ; 3 =  5            #0w
    pla                         ; 4
    pha                         ; 3
    rol     $3f                 ; 5 = 12            free cycles
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
.enterKernel
    sta     WSYNC               ; 3
;---------------------------------------
    bne     .loopKernel         ; 3/2
    rts                         ; 6         @08
KernelCodeEnd

EnterKernel = RAMKernel + .enterKernel - KernelCode
PD = KernelCode - RAMKernel     ; patch delta
  ENDM

; Ideas:
; x 1. use PS as 4th register for color setting: bits 4+5 always 1!
; x 2. use missile for 2nd color: saves one sprite, but requires 2*5 extra cycles, leaving just 6
; x 3. use PS as loop counter: PLA could be used for sprite, Y for preloading
; ? 2.+3. might work



;===============================================================================
; R O M - C O D E
;===============================================================================
    SEG     Bank0
    ORG     BASE_ADR

    NEXT_PASS

    ds      256, $ff

;---------------------------------------------------------------
DrawScreen SUBROUTINE
;---------------------------------------------------------------
.rowCount   = tmpVars
;.carry      = tmpVars+1
.colorPtr   = tmpVars+2
.tmpColP0   = tmpVars+4
.tmpColP1   = tmpVars+5

;    lda     #0
;    bit     SWCHB
;    bvs     .noFlicker
;    lda     frameCnt
;    and     #%1
;.noFlicker
;    sta     .carry

    ldx     #227
.waitTim
    lda     INTIM
    bne     .waitTim
    sta     WSYNC
;---------------------------------------
    sta     VBLANK
    stx     TIM64T              ;  =  7

    lda     #<colorLst_R + NUM_COLS * (NUM_ROWS - 1)
    sta     .colorPtr
    lda     #>colorLst_R
    sta     .colorPtr+1         ;  = 10

    lda     #%01100000
    sta     PF0
    lda     #%11011011
    sta     PF1
    asl                         ;           #%10110110
    sta     PF2                 ;  = 15
;    lda     #%10010010
;    sta     PF0
;    lsr                         ;           #%01001001
;    sta     PF2
;    lsr                         ;           #%00100100
;    sta     PF1                 ;  = 15

    ldy     #NUM_ROWS-1
    sty     .rowCount           ;  = 5
    SLEEP   13
;---------------------------------------------------------------
.loopRows                       ;           @50/51
    clc                         ; 2
    ldy     #NUM_COLS-1         ; 2 =  4
.loopPatch
    lda     (.colorPtr),y       ; 5
    ldx     PatchTbl,y          ; 4
;    adc     .carry              ; 3         flicker two colors together
    sta     $00,x               ; 4
    dey                         ; 2
    bpl     .loopPatch          ; 3/2=21/20
; 4 + (21 - 3) * 13 - 1 = 276 - 39

    lda     .colorPtr
;    sec
    sbc     #NUM_COLS-1
    sta     .colorPtr

    ldy     #15
    lda     #%11000000
    SLEEP   11+NUM_COLS*3
    ldx     .tmpColP1
    stx     COLUP1
    ldx     .tmpColP0
    stx     COLUP0
    sta     GRP0
    sta     GRP1
    jsr     EnterKernel             ; @08
    sty     COLUPF                  ; Y = 0
    sty     GRP1
    lda     targetCol
    sta     COLUP0
; enable target cursor:
    ldx     #%10110110
    lda     .rowCount
    eor     #NUM_ROWS/2
    lsr
    bne     .skipCursorWait
    ldy     #%00111100
    bcc     .skipCursor
    ldx     #%11110110
.skipCursor
    sty     GRP0
    stx     PF2
    dec     .rowCount
    bpl     .loopRows
    sta     WSYNC
;---------------------------------------------------------------
;    ldy     #0
;    sty     PF0
;    sty     PF1
;    sty     PF2
;    sty     GRP0
;    sty     GRP1

    ldx     #2
.waitScreen
    lda     INTIM
    bne     .waitScreen
    sta     WSYNC
;---------------------------------------
    stx     VBLANK
    rts

.skipCursorWait
    nop
    bne     .skipCursor
; /DrawScreen

KernelCode ; patched into
    KERNEL_CODE

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

.mainLoop
    jsr     VerticalBlank
    jsr     DrawScreen
    jsr     OverScan
    jmp     .mainLoop

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

    lda     #64-1
    ldx     #0
    jsr     SetXPos
    lda     #112-1
    inx
    jsr     SetXPos
    sta     WSYNC
;---------------------------------------
    sta     HMOVE

    lda     #%111
    sta     NUSIZ0
    sta     NUSIZ1
    lda     #%1
    sta     CTRLPF

SetupColors
.colorPtrR  = tmpVars
.colorPtrW  = tmpVars+2
.lum        = tmpVars+4

    lda     #>colorLst_W
    sta     .colorPtrW+1
    ldx     #NUM_ROWS-1
.loopRows
    lda     MultTbl,x
    sta     .colorPtrW
    lda     LumTbl,x
    sta     .lum
    ldy     #NUM_COLS-1
.loopColumns
    lda     ColorTbl,y
    ora     .lum
    sta     (.colorPtrW),y
    dey
    bpl     .loopColumns
    dex
    bpl     .loopRows
; 2291 cycles = ~30.2 scanlines

    lda     colorLst_R+NUM_CELLS/2
    sta     targetCol
    rts
; /GameInit

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
    lda     #44
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
    lda     #MOVE_SPEED-1
.dirPressed
    sbc     #MOVE_SPEED-1
    sta     moveSum
    bcs     .skipDirsJmp
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
    ldy     #NUM_CELLS
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
    bne     .loopRowsR
TIM_R
; 2005 cycles
    nop
.skipDirsJmp
    jmp     .skipDirs

;---------------------------------------------------------------
.downR
; move cells down right:
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
    jmp     .loadColumnDown

;---------------------------------------------------------------
.upR
; move cells up right:
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
    bpl     .doLeft
    jmp     .skipLeft

.doLeft
    asl
    bpl     .downL
    asl
    bpl     .upL
; move cells left:
    ldy     #0
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
    cpy     #NUM_CELLS
    bcc     .loopRowsL
TIM_L
; 2117 cycles (.loopColsL crosses page)
    bcs     .skipDirsJmp2

.downL
; move cells down left:
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
    ldx     #NUM_ROWS-2
    sec
.loopLoadUp
    lda     colorBuf_R,x
    sta     colorLst_W,y
    tya
    sbc     #NUM_COLS
    tay
    dex
    bpl     .loopLoadUp
    lda     colorBuf_R+NUM_ROWS-1   ; saved top cell
    sta     colorLst_W,y            ; ...to bottom cell
TIM_LU
; 172 cycles
    bcs     .skipDirs
    DEBUG_BRK

;---------------------------------------------------------------
; check for down:
.skipLeft
    asl
    bmi     .skipDown
; move cells down:
    ldy     #NUM_COLS-1
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
    bcs     .loopColsD
TIM_D
; 1845 cycles
    bcc     .skipDirs
; (2534, 2222, 1897) 1819 cycles = ~23.9 scanlines

;---------------------------------------------------------------
.skipDown
    asl
    bmi     .skipUp
; move cells up:
    ldy     #NUM_CELLS-1
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
    cpy     #NUM_COLS                   ; 2
    bcs     .loopRowsU                  ; 3/2=29/28
    adc     #NUM_CELLS-NUM_COLS-1
    tay
    txa
    sta     colorLst_W-NUM_CELLS+NUM_COLS+1,y
    cpy     #NUM_CELLS-NUM_COLS
    bcs     .loopColsU
TIM_U
; 1826 cycles
    nop
;    jmp     .skipDirs
.skipUp
.skipDirs

    bit     INPT5
    bmi     .skipResetColors
    lda     frameCnt
    and     #$1f
    bne     .skipResetColors
    jsr     SetupColors
.skipResetColors
    rts
; GameCalc

;---------------------------------------------------------------
SaveRightColumn SUBROUTINE
;---------------------------------------------------------------
    ldy     #NUM_CELLS-1
    NOP_W
SaveLeftColumn
    ldy     #NUM_CELLS-NUM_COLS
;SaveColumn
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

;---------------------------------------------------------------
OverScan SUBROUTINE
;---------------------------------------------------------------
  IF NTSC_TIM
    lda     #36
  ELSE
    lda     #63
  ENDIF
    sta     TIM64T

; check for color match:
    bit     SWCHB
    bpl     .coarseCheck
    ldx     #NUM_CELLS/2
    lda     colorLst_R+NUM_CELLS/2
    cmp     targetCol
    bne     .skipNewCol
    beq     .foundTargetCol

.coarseCheck
; check if any cell nearby is close (delta hue OR (todo) delta val <= 2)
;.tmpDiff    = tmpVars
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
    cmp     PrevHueTbl,y
    beq     .checkSameValue
    cmp     NextHueTbl,y
    beq     .checkSameValue
    lda     targetCol                   ; same hue?
    eor     colorLst_R+NUM_CELLS/2
    and     #$f0
    beq     .checkRangeValue            ;  yes, check value +/-2
    bne     .skipNewCol                 ;  no, no match

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
;    beq     .foundTargetCol

;    ldy     #INDEX_LEN-1
;.loopCheck
;    ldx     IndexTbl,y
;    lda     colorLst_R,x
;    cmp     targetCol
;;    beq     .foundTargetCol
;    bne     .nextIndex
;    sbc     colorLst_R+NUM_CELLS/2
;    and     #$0f
;    cmp     #$02+1
;    bcc     .foundTargetCol
;    cmp     #$0e
;    bcs     .foundTargetCol
;.nextIndex
;    dey
;    bpl     .loopCheck
;    bmi     .skipNewCol

.foundTargetCol
  IF REMOVE_CELLS
    lda     #EMPTY_COL
    sta     colorLst_W+NUM_CELLS/2
  ENDIF
  IF SWAP_CELLS
    jsr     GetRandomCellIdx
    lda     colorLst_R+NUM_CELLS/2
    pha
    lda     colorLst_R,x
    sta     colorLst_W+NUM_CELLS/2
    pla
    sta     colorLst_W,x
  ENDIF

    ldy     #2
.loopRandom
    jsr     NextRandom
    and     #$7f
    cmp     #NUM_CELLS
    bcc     .colOk
    dey
    bne     .loopRandom
    lsr
.colOk
    cmp     #NUM_CELLS/2            ; skip current cell
    beq     .loopRandom
    tay
    lda     colorLst_R,y
    sta     targetCol
    lda     #DECAY_LEN
    sta     sound
    lda     #$04
    sta     AUDC0
    lda     #$0e
    sta     AUDF0
.skipNewCol

; continue sound:
    ldy     sound
    beq     .skipSound
    lda     VolumeTbl-1,y
    sta     AUDV0
    dec     sound
.skipSound

.waitTim
    lda     INTIM
    bne     .waitTim
    rts
; /OverScan

;---------------------------------------------------------------
GetRandomCellIdx SUBROUTINE
;---------------------------------------------------------------
    ldx     #INDEX_LEN-1
.loopRandom
    jsr     NextRandom
    and     #$7f
    cmp     #NUM_CELLS
    bcc     .cellOk
    lsr
.cellOk
    cmp     IndexTbl,y
    beq     .loopRandom
    dex
    bne     .loopRandom
    rts

IndexTbl
    .byte   NUM_CELLS/2-NUM_COLS
    .byte   NUM_CELLS/2-1
    .byte   NUM_CELLS/2
    .byte   NUM_CELLS/2+1
    .byte   NUM_CELLS/2+NUM_COLS
INDEX_LEN = . - IndexTbl

VolumeTbl
    .byte   0
    ds      8, 1
    ds      4, 2
    ds      2, 3
    ds      2, 4
    .byte   5, 6, 7, 8, 9, 10
DECAY_LEN = . - VolumeTbl

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

    ALIGN_FREE_LBL  256, "SetXPos"

;---------------------------------------------------------------
SetXPos SUBROUTINE
;---------------------------------------------------------------
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
    .byte   %00001100
    .byte   %00001100
    .byte   %11111110
    .byte   %11111110
    .byte   %11001100
    .byte   %11101100
    .byte   %01101100
    .byte   %01111100
    .byte   %00111100
    .byte   %00111100
FONT_H = . - Four

Seven
    .byte   %01110000
    .byte   %01110000
    .byte   %01110000
    .byte   %01110000
    .byte   %01111000
    .byte   %00111100
    .byte   %00011110
    .byte   %00001110
;    .byte   %01111111
;    .byte   %01111111
Two
    .byte   %11111110
    .byte   %11111110
    .byte   %11110000
    .byte   %01111000
    .byte   %00111100
    .byte   %00011110
    .byte   %00001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Six
    .byte   %01111100
    .byte   %11111110
    .byte   %11100110
    .byte   %11100110
    .byte   %11111110
    .byte   %11111100
    .byte   %11100000
    .byte   %11100110
    .byte   %11111110
;    .byte   %00111110
Three
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %00001110
    .byte   %00111100
    .byte   %00111100
    .byte   %00001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Nine
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %00001110
    .byte   %01111110
    .byte   %11111110
    .byte   %11001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Eight
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %11001110
    .byte   %01111100
    .byte   %01111100
    .byte   %11001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Zero
    .byte   %01111100
    .byte   %11111110
    .byte   %11000110
    .byte   %11000110
    .byte   %11010110
    .byte   %11010110
    .byte   %11000110
    .byte   %11000110
    .byte   %11111110
;    .byte   %00111110
Five
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %00001110
    .byte   %01111110
    .byte   %11111100
    .byte   %11000000
    .byte   %11000000
    .byte   %11111110
    .byte   %11111110

One
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %01111000
    .byte   %01111000
    .byte   %00111000
  CHECKPAGE_DATA_LBL DigitGfx, "DigitGfx"

DigitPtr
    .byte   <Zero, <One, <Two, <Three, <Four
    .byte   <Five, <Six, <Seven, <Eight, <Nine

MultTbl
_IDX    SET 0
    REPEAT NUM_ROWS+1
    .byte  _IDX * NUM_COLS
_IDX    SET _IDX + 1
    REPEND

    .byte   "JTZ"

  IF NTSC_COL
ColorTbl
    .byte   BROWN           ; $20
    .byte   ORANGE          ; $30
    .byte   RED             ; $40
    .byte   MAUVE           ; $50
    .byte   VIOLET          ; $60
    .byte   PURPLE          ; $70
    .byte   BLUE            ; $80
    .byte   BLUE_CYAN       ; $90
    .byte   CYAN            ; $a0
    .byte   CYAN_GREEN      ; $b0
;    .byte   GREEN           ; $c0
    .byte   GREEN_YELLOW    ; $d0
    .byte   GREEN_BEIGE     ; $e0
;    .byte   BEIGE           ; $f0
    .byte   YELLOW          ; $10
NUM_COLS    = . - ColorTbl  ; 13

PrevHueTbl = . - 1
    .byte   GREEN_BEIGE >>4 ; YELLOW        $10
    .byte   YELLOW      >>4 ; BROWN         $20
    .byte   BROWN       >>4 ; ORANGE        $30
    .byte   ORANGE      >>4 ; RED           $40
    .byte   RED         >>4 ; MAUVE         $50
    .byte   MAUVE       >>4 ; VIOLET        $60
    .byte   VIOLET      >>4 ; PURPLE        $70
    .byte   PURPLE      >>4 ; BLUE          $80
    .byte   BLUE        >>4 ; BLUE_CYAN     $90
    .byte   BLUE_CYAN   >>4 ; CYAN          $a0
    .byte   CYAN        >>4 ; CYAN_GREEN    $b0
    .byte   0
    .byte   CYAN_GREEN  >>4 ; GREEN_YELLOW  $d0
    .byte   GREEN_YELLOW>>4 ; GREEN_BEIGE   $e0
NextHueTbl = . - 1
    .byte   BROWN       >>4 ; YELLOW        $10
    .byte   ORANGE      >>4 ; BROWN         $20
    .byte   RED         >>4 ; ORANGE        $30
    .byte   MAUVE       >>4 ; RED           $40
    .byte   VIOLET      >>4 ; MAUVE         $50
    .byte   PURPLE      >>4 ; VIOLET        $60
    .byte   BLUE        >>4 ; PURPLE        $70
    .byte   BLUE_CYAN   >>4 ; BLUE          $80
    .byte   CYAN        >>4 ; BLUE_CYAN     $90
    .byte   CYAN_GREEN  >>4 ; CYAN          $a0
    .byte   GREEN_YELLOW>>4 ; CYAN_GREEN    $b0
    .byte   0
    .byte   GREEN_BEIGE >>4 ; GREEN_YELLOW  $d0
    .byte   YELLOW      >>4 ; GREEN_BEIGE   $e0
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
PrevHueTbl = . - 1
    .byte   GREEN_YELLOW>>4 ; PAL_WHITE     $10
    .byte   PAL_WHITE   >>4 ; YELLOW        $20
    .byte   GREEN       >>4 ; GREEN_YELLOW  $30
    .byte   YELLOW      >>4 ; ORANGE        $40
    .byte   CYAN_GREEN  >>4 ; GREEN         $50
    .byte   ORANGE      >>4 ; RED           $60
    .byte   CYAN        >>4 ; CYAN_GREEN    $70
    .byte   RED         >>4 ; MAUVE         $80
    .byte   BLUE_CYAN   >>4 ; CYAN          $90
    .byte   MAUVE       >>4 ; VIOLET        $a0
    .byte   BLUE        >>4 ; BLUE_CYAN     $b0
    .byte   VIOLET      >>4 ; PURPLE        $c0
    .byte   PURPLE      >>4 ; BLUE          $d0
NextHueTbl = . - 1
    .byte   YELLOW      >>4 ; PAL_WHITE     $10
    .byte   ORANGE      >>4 ; YELLOW        $20
    .byte   PAL_WHITE   >>4 ; GREEN_YELLOW  $30
    .byte   RED         >>4 ; ORANGE        $40
    .byte   GREEN_YELLOW>>4 ; GREEN         $50
    .byte   MAUVE       >>4 ; RED           $60
    .byte   GREEN       >>4 ; CYAN_GREEN    $70
    .byte   VIOLET      >>4 ; MAUVE         $80
    .byte   CYAN_GREEN  >>4 ; CYAN          $90
    .byte   PURPLE      >>4 ; VIOLET        $a0
    .byte   CYAN        >>4 ; BLUE_CYAN     $b0
    .byte   BLUE        >>4 ; PURPLE        $c0
    .byte   BLUE_CYAN   >>4 ; BLUE          $d0
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

