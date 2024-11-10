; Color-Match
; (C) 2024 Thomas Jentzsch

; Ideas:
; - 4 game variations:
;   - ordered colors
;   - random row order
;   - random column order
;   - random cells

;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

VERSION         = $0001
BASE_ADR        = $f000

  IFNCONST TV_MODE ; manually defined here
NTSC            = 0
PAL60           = 1
PAL50           = 0
  ENDIF

F8SC            = 0     ; create F8SC instead of 4KSC (for Harmony)

ILLEGAL         = 1
DEBUG           = 1

SAVEKEY         = 0 ; (-~220) support high scores on SaveKey
PLUSROM         = 0 ; (-~50)

;===============================================================================
; I N C L U D E S
;===============================================================================

    processor 6502
  LIST OFF
    include vcs.h
    include tv_modes.h
  LIST ON


;===============================================================================
; C O L O R - C O N S T A N T S
;===============================================================================

; TODO


;===============================================================================
; G A M E - C O N S T A N T S
;===============================================================================

EOR_RND_LO      = $bd           ; %10110100 ($9c, $b4, $bd, $ca, $eb, $fc)

NUM_ROWS        = 9
NUM_COLS        = 13
NUM_CELLS       = NUM_ROWS * NUM_COLS

MOVE_SPEED      = $20

STACK_SIZE      = 6


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

RAMKernel       ds KernelCodeEnd - KernelCode ; (48 bytes reserved for RAM kernel)
frameCnt        .byte
tmpVars         ds 10
randomLo        .byte
randomHi        .byte
targetCol       .byte
moveSum         .byte

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

  MAC NOP_IMM   ; skip 1 byte, 2 cycles
    .byte   $82
  ENDM

  MAC NOP_B     ; skip 1 byte, 3 cycles
    .byte   $04
  ENDM

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

  MAC SLEEP
    IF {1} = 1
      ECHO "ERROR: SLEEP 1 not allowed !"
      END
    ENDIF
    IF {1} & 1
      nop $00
      REPEAT ({1}-3)/2
        nop
      REPEND
    ELSE
      REPEAT ({1})/2
        nop
      REPEND
    ENDIF
  ENDM

  MAC _CHECKPAGE ; internal, do not use directly
    IF >{1} != >{2}
      ECHO ""
     IF {3} != ""
      ECHO "ERROR: different pages! (", {3}, "=", {2}, ",", {1}, ")"
     ELSE
      ECHO "ERROR: different pages! (", {2}, ",", {1}, ")"
     ENDIF
      ECHO ""
      ERR
    ENDIF
  ENDM

  MAC CHECKPAGE_LBL
    _CHECKPAGE ., {1}, {2}
  ENDM

  MAC CHECKPAGE
    CHECKPAGE_LBL {1}, ""
  ENDM

  MAC CHECKPAGE_DATA_LBL
_ADDR SET . - 1 ; hack to convince DASM
    _CHECKPAGE _ADDR, {1}, {2}
  ENDM

  MAC CHECKPAGE_DATA
    CHECKPAGE_DATA_LBL {1}, ""
  ENDM

  MAC VERSION_STR
    .byte   ((VERSION & $f00) >> 8) + 48
    .byte   "."
    .byte   ((VERSION & $0f0) >> 4) + 48
    .byte   ((VERSION & $00f) >> 0) + 48
  ENDM

;---------------------------------------------------------------
; Free space macros
;---------------------------------------------------------------
ECHO_FREE SET 1     ; 1 = echo free space enabled
FREE_TOTAL SET 0    ; use only once

  MAC OUT_FREE
FREE_GAP$ SET - .
    {1} {2}
FREE_GAP$  SET FREE_GAP$  + .
FREE_TOTAL SET FREE_TOTAL + FREE_GAP$
   IF ECHO_FREE && FREE_GAP$ > 0
    ECHO "@", ., ": Gap:", [FREE_GAP$]d, "; Total:", [FREE_TOTAL]d, ";", {3}, {2}, {4}
   ENDIF
  ENDM

  MAC ALIGN_FREE_LBL
    LIST OFF
    OUT_FREE ALIGN, {1}, "ALIGN", {2}
    LIST ON
  ENDM

  MAC ALIGN_FREE
    LIST OFF
    ALIGN_FREE_LBL {1}, ""
  ENDM

  MAC COND_ALIGN_FREE_LBL ; space required, alignement, "label"
;    LIST OFF
   IF (>(. + {1} - 1)) > (>.)
    ALIGN_FREE_LBL {2}, {3}
   ENDIF
    LIST ON
  ENDM

  MAC COND_ALIGN_FREE ; space required, alignement
;    LIST OFF
    COND_ALIGN_FREE_LBL {1}, {2}, ""
  ENDM

  MAC ORG_FREE_LBL
    LIST OFF
    OUT_FREE ORG, {1}, "ORG", {2}
    LIST ON
  ENDM

  MAC ORG_FREE
    LIST OFF
    ORG_FREE_LBL {1}, ""
  ENDM


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

    ds      256, $55

;---------------------------------------------------------------
DrawScreen SUBROUTINE
;---------------------------------------------------------------
.tmpStack   = tmpVars
.rowCount   = tmpVars+1
.carry      = tmpVars+2
.colorPtr   = tmpVars+3
.tmpColP0   = tmpVars+5
.tmpColP1   = tmpVars+6

    tsx
    stx     .tmpStack

    lda     #%01100000
    sta     PF0
    lda     #%11011011
    sta     PF1
    asl                     ; #%10110110
    sta     PF2
;    lda     #%10010010
;    sta     PF0
;    lsr                     ; #%01001001
;    sta     PF2
;    lsr                     ; #%00100100
;    sta     PF1

    lda     #<colorLst_R + NUM_COLS * (NUM_ROWS - 1)
    sta     .colorPtr
    lda     #>colorLst_R
    sta     .colorPtr+1

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
    stx     TIM64T

    ldy     #NUM_ROWS-1
    sty     .rowCount
    SLEEP   38
;---------------------------------------------------------------
.loopRows
;    sta     WSYNC
;---------------------------------------
    clc                         ; 2
    ldy     #NUM_COLS-1         ; 2 =  4
.loopPatch
    lda     (.colorPtr),y       ; 5
    ldx     PatchTbl,y          ; 4
;    adc     .carry              ; 3
    sta     $00,x               ; 4
    dey                         ; 2
    bpl     .loopPatch          ; 3/2=21/20
; 4 + 21 * 13 - 1 = 276

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
; enable focus
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
    sty     PF0                     ; Y = 0
    sty     PF1
    sty     PF2
    sty     GRP0
    sty     GRP1

    ldx     #2
.waitScreen
    lda     INTIM
    bne     .waitScreen
    sta     WSYNC
;---------------------------------------
    stx     VBLANK

    ldx     .tmpStack
    txs
    rts

.skipCursorWait
    nop
    bne     .skipCursor
; /DrawScreen

PatchTbl
    .byte   Col0 + 1 - PD
    .byte   Col1 + 1 - PD
    .byte   Col2 + 1 - PD
    .byte   Col3 + 1 - PD
    .byte   Col4 + 1 - PD
    .byte   .tmpColP0 ;COLUP0
    .byte   Col6 + 1 - PD
    .byte   Col7 + 1 - PD
    .byte   Col8 + 1 - PD
    .byte   .tmpColP1 ;COLUP1
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
    jsr     GameCalc
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

;    lda     #RED+2
;    sta     COLUPF
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

    rts
; VerticalBlank

;---------------------------------------------------------------
GameCalc SUBROUTINE
;---------------------------------------------------------------
.colorPtrR  = tmpVars
.colorPtrW  = tmpVars+2

DEBUG0
    lda     moveSum
    ldx     SWCHA
    cpx     #$ff
    bcc     .dirPressed
    lda     #MOVE_SPEED-1
.dirPressed
    sbc     #MOVE_SPEED-1
    sta     moveSum
    bcs     .skipDirsJmp
    lda     #>colorLst_W
    sta     .colorPtrW+1
    sta     .colorPtrR+1
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
; right
    ldx     #NUM_ROWS-1
.loopRowsR
    lda     MultTbl,x
    sta     .colorPtrW
    ora     #R_OFS
    tay
    dey
    sty     .colorPtrR
    ldy     #NUM_COLS
    lda     (.colorPtrR),y      ;               R+x
    pha
    dey
.loopColsR
    lda     (.colorPtrR),y      ; 5             R
    sta     (.colorPtrW),y      ; 6             W+1
    dey                         ; 2
    bne     .loopColsR          ; 3/2=14/13
    pla
    sta     (.colorPtrW),y      ; 6
    dex
    bpl     .loopRowsR
    bmi     .skipDirsJmp

;---------------------------------------------------------------
.downR
; save right column:
    jsr     SaveRightColumn
 ; move cells down right:
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
    jsr     LoadColumnDown
.skipDirsJmp
    jmp     .skipDirs

;---------------------------------------------------------------
.upR
; save right column:
    jsr     SaveRightColumn
; move cells up right:
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
    jsr     LoadColumnUp
DEBUG2
;    jmp     .skipDirs
    bne     .skipDirsJmp
    DEBUG_BRK

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
; shift colors left
    ldx     #NUM_ROWS-1
.loopRowsL
    lda     MultTbl,x
    sec
    sbc     #1
    sta     .colorPtrW
  lda     #>colorLst_W
  sbc     #0
  sta     .colorPtrW+1
    lda     MultTbl,x
    ora     #R_OFS
    sta     .colorPtrR

    ldy     #0
    lda     (.colorPtrR),y      ;               R
    pha
    iny
.loopColsL
    lda     (.colorPtrR),y      ; 5             R+1
    sta     (.colorPtrW),y      ; 6             W
    iny                         ; 2
    cpy     #NUM_COLS
    bcc     .loopColsL          ; 3/2=14/13
    pla
    sta     (.colorPtrW),y      ; 6             W
    dex
    bpl     .loopRowsL
    bmi     .skipDirsJmp

.downL
DEBUG3
; move cells down left:
    jsr     SaveLeftColumn
    ldy     #0
.loopColsDL
    clc
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
    bcc     .loopRowsDL                 ; 3/2=29/28
    sbc     #NUM_CELLS-NUM_COLS-1
    tay
    txa
    sta     colorLst_W+NUM_CELLS-NUM_COLS-1,y
    cpy     #NUM_COLS-1
    bcc     .loopColsDL
    ldy     #NUM_CELLS-NUM_COLS-1
    jsr     LoadColumnDown
    jmp     .skipDirs

;---------------------------------------------------------------
.upL
; save left column:
    jsr     SaveLeftColumn
; move cells:
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
    jsr     LoadColumnUp
    bne     .skipDirs
    DEBUG_BRK

;---------------------------------------------------------------
; check for down:
.skipLeft
    asl
    bmi     .skipDown
; shift colors down:
    ldy     #NUM_COLS-1
.loopColsD
    clc
    ldx     colorLst_R,y
.loopRowsD
    lda     colorLst_R+NUM_COLS*1,y ; 4
    sta     colorLst_W+NUM_COLS*0,y ; 5
    lda     colorLst_R+NUM_COLS*2,y ; 4
    sta     colorLst_W+NUM_COLS*1,y ; 5
    tya                             ; 2
    adc     #NUM_COLS*2             ; 2
    tay                             ; 2
    cpy     #NUM_CELLS-NUM_COLS     ; 2
    bcc     .loopRowsD              ; 3/2=29/28
    txa
    sta     colorLst_W,y
    tya
    sbc     #NUM_CELLS-NUM_COLS+1
    tay
;    txa
;    sta     colorLst_W+NUM_CELLS-NUM_COLS+1,y      ; wraps and causes RW-Trap!
    bcs     .loopColsD
    bcc     .skipDirs
; (2534, 2222, 1897) 1819 cycles = ~23.9 scanlines

;---------------------------------------------------------------
.skipDown
    asl
    bmi     .skipUp
; shift colors up:
    ldy     #NUM_CELLS-1
    sec
.loopColsU
    ldx     colorLst_R,y
.loopRowsU
    lda     colorLst_R-NUM_COLS*1,y ; 4
    sta     colorLst_W-NUM_COLS*0,y ; 5
    lda     colorLst_R-NUM_COLS*2,y ; 4
    sta     colorLst_W-NUM_COLS*1,y ; 5
    tya                             ; 2
    sbc     #NUM_COLS*2             ; 2
    tay                             ; 2
    cpy     #NUM_COLS               ; 2
    bcs     .loopRowsU              ; 3/2=29/28
    adc     #NUM_CELLS-NUM_COLS-1
    tay
    txa
    sta     colorLst_W-NUM_CELLS+NUM_COLS+1,y
    cpy     #NUM_CELLS-NUM_COLS
    bcs     .loopColsU
;    jmp     .skipDirs
; (2534, 2222, 1897) 1776 cycles = ~23.4 scanlines
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
LoadColumnDown SUBROUTINE
;---------------------------------------------------------------
    lda     colorBuf_R
    sta     colorLst_W+NUM_COLS,y
    ldx     #NUM_ROWS-1
    sec
.loopLoad
    lda     colorBuf_R,x
    sta     colorLst_W,y
    tya
    sbc     #NUM_COLS
    tay
    dex
    bne     .loopLoad
    rts

;---------------------------------------------------------------
LoadColumnUp SUBROUTINE
;---------------------------------------------------------------
    ldx     #NUM_ROWS-2
    sec
.loopLoad
    lda     colorBuf_R,x
    sta     colorLst_W,y
    tya
    sbc     #NUM_COLS
    tay
    dex
    bpl     .loopLoad
    lda     colorBuf_R+NUM_ROWS-1
    sta     colorLst_W,y
    rts

;---------------------------------------------------------------
KernelCode SUBROUTINE       ;               patched into
;---------------------------------------------------------------
    KERNEL_CODE

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
    ldx     #0
    lda     colorLst_R+NUM_CELLS/2
    cmp     targetCol
    bne     .skipNewCol
    jsr     NextRandom
    and     #$7f
    cmp     #NUM_CELLS
    bcc     .colOk
    lsr
.colOk
    tay
    lda     colorLst_R,y
    sta     targetCol
    ldx     #$0e            ; flash
.skipNewCol
    stx     COLUBK

.waitTim
    lda     INTIM
    bne     .waitTim
    rts
; OverScan

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

ColorTbl
  IF NTSC_COL
    .byte   BROWN
    .byte   ORANGE
    .byte   RED
    .byte   MAUVE
    .byte   VIOLET
    .byte   PURPLE
    .byte   BLUE
    .byte   BLUE_CYAN
    .byte   CYAN
    .byte   CYAN_GREEN
;    .byte   GREEN
    .byte   GREEN_YELLOW
    .byte   GREEN_BEIGE
;    .byte   BEIGE
    .byte   YELLOW
  ELSE
    .byte   YELLOW
    .byte   ORANGE
    .byte   RED
    .byte   MAUVE
    .byte   VIOLET
    .byte   PURPLE
    .byte   BLUE
    .byte   BLUE_CYAN
    .byte   CYAN
    .byte   CYAN_GREEN
    .byte   GREEN
    .byte   GREEN_YELLOW
    .byte   BLACK
  ENDIF
NUM_COLS    = . - ColorTbl  ; 13

LumTbl
;    .byte   $04
;    .byte   $06
;    .byte   $08
;    .byte   $0a
;    .byte   $0e
;    .byte   $0a
;    .byte   $06
;    .byte   $04
;    .byte   $02

    .byte   $00|1   ; avoid $00 by adding 1
    .byte   $00|1
    .byte   $02
    .byte   $04
    .byte   $06
    .byte   $08
    .byte   $0a
    .byte   $0c
    .byte   $0e

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

