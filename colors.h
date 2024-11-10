; general color constants
BLACK           = $00
WHITE           = $0e
  IF NTSC_COL
YELLOW          = $10
BROWN           = $20
ORANGE          = $30
RED             = $40
MAUVE           = $50
VIOLET          = $60
PURPLE          = $70
BLUE            = $80
BLUE_CYAN       = $90
CYAN            = $a0
CYAN_GREEN      = $b0
GREEN           = $c0
GREEN_YELLOW    = $d0
GREEN_BEIGE     = $e0
BEIGE           = $f0
  ELSE
YELLOW          = $20
BROWN           = YELLOW
ORANGE          = $40
RED             = $60
MAUVE           = $80
VIOLET          = $a0
PURPLE          = $c0
BLUE            = $d0
BLUE_CYAN       = $b0
CYAN            = $90
CYAN_GREEN      = $70
GREEN           = $50
GREEN_YELLOW    = $30
GREEN_BEIGE     = GREEN_YELLOW
BEIGE           = YELLOW
  ENDIF