.segment "HEADER"
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $01 ; 1 * 8KB CHR ROM
.byte %00000000 ; mapper and mirroring
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00 ; filler bytes
.segment "ZEROPAGE" ; LSB 0 - FF

background:       .res 2 ; reserve 2 bytes of space for background
gamestate:        .res 1 ; .rs 1 means reserve one byte of space
playerx:          .res 1 ; player horizontal position
playery:          .res 1 ; player vertical position
playerattributes: .res 1 ; attributes for the player sprites
onground:         .res 1 ; 1=player is on the ground
animationframe:   .res 1 ; used for animation when moving
playerspeedx:     .res 1 ; player horizontal speed per frame
playerspeedy:     .res 1 ; player vertical speed per frame
buttons1:         .res 1 ; player 1 gamepad buttons, one bit per button

;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
; when player reaches one of these prevent further movement
RIGHTWALL      = $E8  
TOPWALL        = $04  
BOTTOMWALL     = $C0
LEFTWALL       = $08
PLATAFORM      = $80
PIPETOP        = $A0
PIPEWALL       = $CB



.segment "STARTUP"
RESET:
  SEI         ; Disables IRQs
  CLD         ; disable decimal mode
  LDX #$40    
  STX $4017   ;disable APU frame IRQ
  LDX #$FF    
  TXS         ; Set up stack
  INX         ; X = 0
  STX $2000   ; disable NMI
  STX $2001   ; disable rendering
  STX $4010   ; disable DMC IQRs

vblankwait1:    ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  STA $0000, X 
  STA $0100, X 
  STA $0300, X
  STA $0400, X
  STA $0500, X
  STA $0600, X
  STA $0700, X
  LDA #$FE
  STA $0200, X
  LDA #$00
  INX
  BNE clrmem    

vblankwait2:     ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2

LoadPalettes:
  LDA $2002         ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006         ; write the high byte of $3F00 address
  LDA #$00
  STA $2006         ; write the low byte of $3F00 address
  LDX #$00          ; start out at 0
LPLoop:
  LDA palette, x    ; load data from address (palette + the value in x)
  STA $2007         ; write to PPU
  INX                   
  CPX #$20          ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LPLoop        ; Branch to LoadPalettesLoop if compare was Not Equal to zero

LoadBackground:
  LDA #<BackgroundData
  STA background
  LDA #>BackgroundData
  STA background+1
  BIT $2002       ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006       ; write the high byte of $2000 address
  LDA #$00
  STA $2006       ; write the low byte of $2000 address
  LDX #$00        ; start out at 0
  LDY #$00        ; start out at 0
LBLoop:
  LDA (background), Y
  STA $2007
  INY
  CPX #$03
  BNE :+
  CPY #$C0
  BEQ DoneLoadingBackground
  :
  CPY #$00
  BNE LBLoop
  INX
  INC background+1
  JMP LBLoop
DoneLoadingBackground:
  LDX #$00  ; start at 0
  LDY #$00  ; start at 0

LoadAttribute:
  LDA $2002         ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006         ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006         ; write the low byte of $23C0 address
  LDX #$00          ; start out at 0
LALoop:
  LDA attribute, x  ; load data from address (attribute + the value in x)
  STA $2007         ; write to PPU
  INX                   
  CPX #$40          ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LALoop        ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                    ; if compare was equal to 128, keep going down
                    
LoadSprites:
  LDX #$00          ; start at 0
LSLoop:
  LDA sprites, x    ; load data from address (sprites + x)
  STA $0200, x      ; store into RAM address ($0200 + x)
  INX
  CPX #$A0          ; Compare X to hex $10, decimal 16
  BNE LSLoop        ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                    ; if compare was equal to 16, keep going down

; Set initial player stats
  LDA #$50
  STA playery 

  LDA #$80
  STA playerx

  LDA #%00000011
  STA playerattributes

  LDA #$01
  STA playerspeedx
  
  LDA #$02
  STA playerspeedy

  LDA #$00
  STA animationframe

; Starting game state
  LDA #STATEPLAYING
  STA gamestate

  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
    JMP Forever ; jump back to Forever, infinite loop
  
NMI:
  LDA #$00
  STA $2003   ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014   ; set the high byte (02) of the RAM address, start the transfer


; This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000  ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110  ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ; tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  JSR ReadController1  ;;get the current button data for player 1
  
GameEngine:  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying   ; game is playing
GameEngineDone:  
  
  JSR UpdateSprites  ; set player sprites from positions

  RTI             ; return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
EnginePlaying:

PlayerRun:
  ; Check if B button is pressed
  LDA buttons1
  AND #%01000000
  CMP #$00
  BNE Run
  Walk:
    LDA #$01
    STA playerspeedx
    JMP PlayerRunDone
  Run:
    LDA #$02
    STA playerspeedx
PlayerRunDone:

MovePlayerRight:
  ; Check if right button is pressed
  LDA buttons1
  AND #%00000001
  CMP #$00
  BEQ MovePlayerRightDone
  ; Set atrributes to face right
  LDA #%00000011
  STA playerattributes
  ; Set animation frame
  LDA animationframe
  CLC
  ADC #$01
  STA animationframe
  CMP #$09
  BNE UpdatePlayerXRight
  LDA #$00
  STA animationframe
  ; Player moves right
UpdatePlayerXRight:
  LDA playerx
  CLC
  ADC playerspeedx
  STA playerx
  ; COLLISIONS
  CMP #RIGHTWALL
  BCC PipeWall
  LDA #RIGHTWALL
  STA playerx
  JMP MovePlayerRightDone
  PipeWall:
    LDA playery
    CMP #$A4
    BCC MovePlayerRightDone
    LDA playerx
    CMP #PIPEWALL
    BCC MovePlayerRightDone
    LDA #PIPEWALL
    STA playerx
MovePlayerRightDone:
  

MovePlayerLeft:
  ; Check if left button is pressed
  LDA buttons1
  AND #%00000010
  CMP #$00
  BEQ MovePlayerLeftDone
  ; Set atrributes to face right
  LDA #%01000011
  STA playerattributes
  ; Set animation frame
  LDA animationframe
  CLC
  ADC #$01
  STA animationframe
  CMP #$09
  BNE UpdatePlayerXLeft
  LDA #$00
  STA animationframe
  ; Player moves right
UpdatePlayerXLeft:
  LDA playerx
  SEC
  SBC playerspeedx
  STA playerx
  ; COLLISIONS
  CMP #LEFTWALL
  BCS MovePlayerLeftDone
  LDA #LEFTWALL
  STA playerx
MovePlayerLeftDone:


PlayerJump:
  LDA onground  ; onground=0, skip this section
  CMP #$00
  BEQ PlayerJumpDone
  ; Check if A button is pressed
  LDA buttons1
  AND #%10000000
  CMP #$00
  BEQ PlayerJumpDone
  ; JUMP!
  LDA playery
  SEC
  SBC #$48
  STA playery
  ; Player no longer on the ground
   LDA #$00
   STA onground
  ; COLLISIONS
  LDA playery
  CMP #TOPWALL
  BCS PlayerJumpDone
  LDA #TOPWALL
  STA playery

PlayerJumpDone:


Gravity:
  ; check if players vertical position is = to the plataform
  LDA playery
  CMP #PLATAFORM
  BEQ OutsidePlataform
  ; check if the players veritcal position is = to the pipe's top
  LDA playery
  CMP #PIPETOP
  BEQ OutsidePipe
  JMP CheckForGround

OutsidePlataform:
  ; turn gravaty back on if the player walks off the plataform
  LDA playerx
  CMP #$40
  BCC GravityON
  LDA playerx
  CMP #$A0
  BCS GravityON
  JMP CheckForGround
  ; turn gravity back on if the player walks off the pipe's top
OutsidePipe:
  LDA playerx
  CMP #PIPEWALL
  BCC GravityON
  JMP CheckForGround

GravityON:
  ; onground=0, thus the player is in the air whcich means we should turn gravity back on
  LDA #$00
  STA onground

CheckForGround:
  LDA onground  ; onground=1, skip this section
  CMP #$00
  BEQ Fall
  JMP GravityDone
  ; Move player down
Fall:
  LDA playery
  CLC
  ADC playerspeedy
  STA playery
  ; COLLISIONS
Ground:
  CMP #BOTTOMWALL
  BCC PipeTop
  LDA #BOTTOMWALL
  STA playery
  JMP NotFalling
PipeTop:
  LDA playerx
  CMP #$CD
  BCC OnPlataform
  LDA playery
  CMP #PIPETOP
  BCC OnPlataform
  LDA #PIPETOP
  STA playery
  JMP NotFalling
OnPlataform:
  LDA playerx
  CMP #$40
  BCC GravityDone
  LDA playerx
  CMP #$A0
  BCS GravityDone
  LDA playery
  CMP #PLATAFORM
  BNE GravityDone
  LDA #PLATAFORM
  STA playery

NotFalling: 
  ; onground=1, thus the player is grounded
  LDA #$01
  STA onground

GravityDone:
  
  JMP GameEngineDone
  
UpdateSprites:
;   LDA playery  ;update all player sprite info
;   STA $0200
;   STA $0204
;   CLC
;   ADC #08
;   STA $0208
;   STA $020C

;   LDA playerattributes
;   AND #%01000000
;   CMP #$00
;   BNE FacingLeft
;   JMP FacingRight
; FacingLeft:
;   LDA onground
;   BNE PlayerOnGroundLeft
; PlayerJumpingLeft:
;   LDA #$41
;   STA $0201
;   LDA #$32
;   STA $0205
;   LDA #$43
;   STA $0209
;   LDA #$42
;   STA $020D
;   JMP SetAtrributes
; PlayerOnGroundLeft:
;   LDA buttons1
;   AND #%00000010
;   CMP #$00
;   BNE PlayerMovingLeft
; PlayerIdleLeft:
;   LDA #$33
;   STA $0201
;   LDA #$32
;   STA $0205
;   LDA #$4F
;   STA $0209
;   STA $020D
;   JMP SetAtrributes
; PlayerMovingLeft:
;   LDA animationframe
;   CMP #$08
;   BEQ Frame3L
;   LDA animationframe
;   CMP #$04
;   BCC Frame2L
;   LDA animationframe
;   CMP #$00
;   BCC Frame1L  
; Frame1L:
;   LDA #$33
;   STA $0201
;   LDA #$32
;   STA $0205
;   LDA #$35
;   STA $0209
;   LDA #$34
;   STA $020D
;   JMP SetAtrributes
; Frame2L:
;   LDA #$37
;   STA $0201
;   LDA #$36
;   STA $0205
;   LDA #$39
;   STA $0209
;   LDA #$38
;   STA $020D
;   JMP SetAtrributes
; Frame3L:
;   LDA #$37
;   STA $0201
;   LDA #$3A
;   STA $0205
;   LDA #$3C
;   STA $0209
;   LDA #$3B
;   STA $020D
;   JMP SetAtrributes

; FacingRight:
;   LDA onground
;   BNE PlayerOnGroundRight
; PlayerJumpingRight:
;   LDA #$32
;   STA $0201
;   LDA #$41
;   STA $0205
;   LDA #$42
;   STA $0209
;   LDA #$43
;   STA $020D
;   JMP SetAtrributes
; PlayerOnGroundRight:
;   LDA buttons1
;   AND #%00000001
;   CMP #$00
;   BNE PlayerMovingRight
; PlayerIdleRight:
;   LDA #$32
;   STA $0201
;   LDA #$33
;   STA $0205
;   LDA #$4F
;   STA $0209
;   STA $020D
;   JMP SetAtrributes   
; PlayerMovingRight:
;   LDA animationframe
;   CMP #$08
;   BEQ Frame3R
;   LDA animationframe
;   CMP #$04
;   BCC Frame2R
;   LDA animationframe
;   CMP #$00
;   BCC Frame1R
; Frame1R:
;   LDA #$32
;   STA $0201
;   LDA #$33
;   STA $0205
;   LDA #$34
;   STA $0209
;   LDA #$35
;   STA $020D
;   JMP SetAtrributes
; Frame2R:
;   LDA #$36
;   STA $0201
;   LDA #$37
;   STA $0205
;   LDA #$38
;   STA $0209
;   LDA #$39
;   STA $020D
;   JMP SetAtrributes
; Frame3R:
;   LDA #$3A
;   STA $0201
;   LDA #$37
;   STA $0205
;   LDA #$3B
;   STA $0209
;   LDA #$3C
;   STA $020D
;   JMP SetAtrributes
  
; SetAtrributes:
;   LDA playerattributes
;   STA $0202
;   STA $0206
;   LDA onground
;   BEQ PlayerNotIdle
;   LDA buttons1
;   AND #%00000011
;   CMP #$00
;   BEQ PlayerIdle
; PlayerNotIdle:
;   LDA playerattributes
;   STA $020A
;   STA $020E
;   JMP SetPlayerX
; PlayerIdle:
;   LDA #%00000011
;   STA $020A
;   LDA #%01000011
;   STA $020E

; SetPlayerX:
;   LDA playerx
;   STA $0203
;   STA $020B
;   CLC
;   ADC #$08
;   STA $0207
;   STA $020F
  
   RTS

ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016         ; $4016 -> A | B | Sel | Sta | Up | Down | Left | Right
  LSR A             ; bit0 -> Carry
  ROL buttons1      ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
  

    
  RTI             ; return from interrupt

palette:
  .byte $22,$29,$1A,$0F ,$22,$36,$17,$0F ,$22,$30,$21,$0F, $22,$27,$17,$0F  ; background palette 
  .byte $22,$1C,$2B,$39, $22,$06,$15,$36, $22,$05,$26,$30, $22,$30,$36,$2C  ; sprite palette 

sprites:        
    .byte $80, $32, %00000011, $7C   ; sprite 0
    .byte $80, $33, %00000011, $84   ; sprite 1
    .byte $88, $4F, %00000011, $7C   ; sprite 2
    .byte $88, $4F, %01000011, $84   ; sprite 3

    .byte $90, $32, %00000011, $7C   
    .byte $90, $33, %00000011, $84   
    .byte $98, $34, %00000011, $7C   
    .byte $98, $35, %00000011, $84   

    .byte $A0, $36, %00000011, $7C   
    .byte $A0, $37, %00000011, $84   
    .byte $A8, $38, %00000011, $7C   
    .byte $A8, $39, %00000011, $84   

    .byte $B0, $3A, %00000011, $7C   
    .byte $B0, $37, %00000011, $84   
    .byte $B8, $3B, %00000011, $7C   
    .byte $B8, $3C, %00000011, $84   

    .byte $C0, $32, %00000011, $7C   
    .byte $C0, $41, %00000011, $84   
    .byte $C8, $42, %00000011, $7C   
    .byte $C8, $43, %00000011, $84   


    .byte $80, $33, %01000011, $90   
    .byte $80, $32, %01000011, $98   
    .byte $88, $4F, %00000011, $90   
    .byte $88, $4F, %01000011, $98   

    .byte $90, $33, %01000011, $90   
    .byte $90, $32, %01000011, $98   
    .byte $98, $35, %01000011, $90   
    .byte $98, $34, %01000011, $98   

    .byte $A0, $37, %01000011, $90   
    .byte $A0, $36, %01000011, $98   
    .byte $A8, $39, %01000011, $90   
    .byte $A8, $38, %01000011, $98   

    .byte $B0, $37, %01000011, $90   
    .byte $B0, $3A, %01000011, $98   
    .byte $B8, $3C, %01000011, $90  
    .byte $B8, $3B, %01000011, $98   

    .byte $C0, $41, %01000011, $90   
    .byte $C0, $32, %01000011, $98   
    .byte $C8, $43, %01000011, $90   
    .byte $C8, $42, %01000011, $98   

BackgroundData:
    .incbin "background.bin"
      
attribute:
    .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
    .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
    .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %01010000, %01010000, %01010000, %01010000, %01010000, %01010000, %01010000, %01010000
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101

.segment "VECTORS"
    .word NMI
    .word RESET
    .word 0
    ; 
.segment "CHARS"
    .incbin "mario.chr"