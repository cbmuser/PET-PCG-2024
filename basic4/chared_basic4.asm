!to"charedit",cbm
;
; lots of offsets and variables
s_lo         = $0a
s_hi         = $0b
t_lo         = $0c
t_hi         = $0d
temp         = $54
lines        = $55
column       = $56
row          = $57
c_lo         = $58
c_hi         = $59
e_lo         = $5a
e_hi         = $5b
e_column     = $5c
e_row        = $5d
; file handling
status       = $96
fnadr        = $da
fnlen        = $d1
la           = $d2 
sa           = $d3 
fa           = $d4
open         = $f563
talk         = $f0d2
listen       = $f0d5
unlisn       = $f1c0
basin        = $ffcf
secnd        = $f2c1
load         = $f356
save         = $f6e3
close        = $f2e0  
; screen and char
screen_ram   = $8000
char         = $1500
cur_col      = $c6
cur_row      = $d8
bsout        = $ffd2

*= $0400
!byte $00,$0c,$08,$0a,$00,$9e,$31,$30,$33,$39,$00,$00,$00,$00
; main
*=$040f
          lda #$0e
          sta $e84c 
          lda #$93
          jsr bsout 
          lda #$00
          sta row 
          tay
          tax
          sta t_lo
          lda #$80
          sta t_hi
          lda #<screen
          sta s_lo
          lda #>screen
          sta s_hi
-         lda (s_lo),y
          sta (t_lo),y
          iny
          bne -  
          inc t_hi
          inc s_hi
          inx
          cpx #$03
          bne -     
          ldy #$00
          tya
-         sta $1500,y
          sta $1600,y           
          iny
          bne - 
          lda #<char
          sta s_lo
          lda #>char
          sta s_hi
          lda #$00
          sta t_lo
          lda #$a0
          sta t_hi
          jsr write_charset
          lda #<char
          sta s_lo
          lda #>char
          sta s_hi
          clc  
          lda s_lo
          adc #$08 
          sta s_lo
          bcc +
          inc s_hi 
+         jsr show_charset
          lda #$1e  
          sta $8374
          lda #$00
          sta column 
          lda #$74
          sta c_lo
          lda #$83
          sta c_hi
          lda #<char
          sta s_lo
          lda #>char
          sta s_hi
          jsr show_char
          jmp keys

;--------------------------------------------------------------
; update screen
;--------------------------------------------------------------
write_new:
          lda s_lo
          pha
          lda s_hi
          pha
          lda #<char
          sta s_lo
          lda #>char
          sta s_hi
          lda #$00
          sta t_lo
          lda #$a0
          sta t_hi
          jsr write_charset
          pla
          sta s_hi
          pla
          sta s_lo  
          jsr show_charset
          jmp keys 
;--------------------------------------------------------------
; enter filename for load or save 
;--------------------------------------------------------------
new_filename: 

          lda #$cf
          sta $c4            ; Pointer: Current Screen	Line Address
          lda #$82
          sta $c5    
          lda #<filename 
          sta fnadr   
          lda #>filename 
          sta fnadr+1
          lda #$12
          sta cur_row
          lda #$0a
          sta cur_col    
          ldy #$00
          sty fnlen     
-         jsr basin
          cmp #$0d           
          beq +
          sta (fnadr),y
          iny
          sty fnlen
          bne -
+         rts         
;--------------------------------------------------------------
; save charset
;--------------------------------------------------------------           
savefile: jsr new_filename
          jsr charset_save
          jmp keys
;--------------------------------------------------------------
; load charset
;--------------------------------------------------------------
loadfile:         
          jsr new_filename
          jsr charset_load 
          lda #<char
          sta s_lo
          lda #>char
          sta s_hi
          lda #$00
          sta t_lo
          lda #$a0
          sta t_hi
          jsr write_charset
          lda #<char
          sta s_lo
          lda #>char
          sta s_hi
          clc  
          lda s_lo
          adc #$08 
          sta s_lo
          bcc +
          inc s_hi 
+         jsr show_charset
          lda column
          tay  
          lda #$20           
          sta (c_lo),y
          lda #$1e  
          sta $8374
          lda #$00
          sta column 
          sta row
          lda #$74
          sta c_lo
          lda #$83
          sta c_hi
          lda #<char
          sta s_lo
          lda #>char
          sta s_hi
          jsr show_char
          jmp keys
;--------------------------------------------------------------
; keyboard and long jumps
;--------------------------------------------------------------
j_savefile:   jmp savefile 
j_loadfile:   jmp loadfile
j_write_new:  jmp write_new
j_edit_char:  jmp edit_char 
j_move_right: jmp move_right 
j_move_up:    jmp move_up 
j_directory   jmp directory
keys:
          jsr $ffe4
          cmp #"6"
          beq j_move_right
          cmp #"4"
          beq move_left
          cmp #"2"
          beq move_down
          cmp #"8"
          beq j_move_up
          cmp #"E"
          beq j_edit_char
          cmp #"W"
          beq j_write_new
          cmp #"D"
          beq j_directory
          cmp #"L"
          beq j_loadfile
          cmp #"S"            
          beq j_savefile
          cmp #"Q"
          beq +
          jmp keys
+         lda #$93
          jsr bsout 
          rts
;--------------------------------------------------------------
; move right charlist
;--------------------------------------------------------------
move_right:
          lda column
          cmp #$1f
          beq ++
          lda column
          tay 
          lda #$20
          sta (c_lo),y 
          iny
          lda #$1e
          sta (c_lo),y
          sty column         
          clc 
          lda s_lo
          adc #$08           
          sta s_lo
          bcc +
          inc s_hi
+         jsr show_char   
++        jmp keys
;--------------------------------------------------------------
; move left charlist
;--------------------------------------------------------------
move_left:
          lda column
          beq ++
          lda column
          tay 
          lda #$20
          sta (c_lo),y 
          dey
          lda #$1e
          sta (c_lo),y
          sty column            
          sec 
          lda s_lo
          sbc #$08           
          sta s_lo
          bcs +
          dec s_hi
+         jsr show_char   
++        jmp keys
;--------------------------------------------------------------
; move down charlist
;-------------------------------------------------------------- 
move_down:
          lda row
          bne ++   
          inc row
          lda column 
          tay 
          lda #$20
          sta (c_lo),y 
          ldy #$00
          sty column
          clc
          lda c_lo
          adc #$50
          sta c_lo                
          bcc +
          inc c_hi
+         lda #$1e
          sta (c_lo),y
          inc s_hi
          lda #$00
          sta s_lo
          jsr show_char 
++        jmp keys
;--------------------------------------------------------------
; move up charlist
;--------------------------------------------------------------
move_up:
          lda row
          beq ++   
          dec row
          lda column 
          tay 
          lda #$20
          sta (c_lo),y 
          ldy #$00
          sty column
          sec
          lda c_lo
          sbc #$50
          sta c_lo                
          bcs +
          dec c_hi
+         lda #$1e
          sta (c_lo),y
          dec s_hi
          lda #$00
          sta s_lo
          jsr show_char 
++        jmp keys
;--------------------------------------------------------------
; write charset to adapter
;--------------------------------------------------------------
write_charset:
         ldx #$00
--       ldy #$00
-        lda (s_lo),y
         sta (t_lo),y
         iny
         bne - 
         inc s_hi  
         inc t_hi  
         inx
         cpx #$02
         bne --
         rts
;--------------------------------------------------------------
; show charset lines
;--------------------------------------------------------------
show_charset:
         ldx #$40
         ldy #$00  
         lda #$83
         sta t_hi
         lda #$4c
         sta t_lo 
-        txa
         sta (t_lo),y 
         inx 
         iny
         cpy #$20
         bne - 
         clc
         lda t_lo
         adc #$30
         sta t_lo
         bcc j
         inc t_hi
j:       txa
         sta (t_lo),y 
         inx 
         iny
         cpy #$40
         bne j 
         rts  
;--------------------------------------------------------------
; show char in editor-window
;--------------------------------------------------------------
show_char:
         lda #$80
         sta t_hi
         lda #$cb
         sta t_lo 
         ldy #00   
         lda #$07 
         sta lines 
         lda (s_lo),y           
         sta temp
         tya
         pha       
         jsr show_byte
-        pla
         tay
         iny
         lda (s_lo),y           
         sta temp
         clc
         lda t_lo
         adc #$28    
         sta t_lo 
         bcc +
         inc t_hi
+        tya
         pha
         jsr show_byte
         dec lines 
         lda lines
         bne -             
         pla
         rts
;--------------------------------------------------------------
; build char in editor-window
;--------------------------------------------------------------
show_byte:            
         ldy #$07  
-        lda temp 
         and tab,y
         beq spc
         lda #$a0
         sta (t_lo),y
         bne +   
spc:     lda #$20
         sta (t_lo),y
+        dey
         bpl -
         rts
;--------------------------------------------------------------
; exit the edit-window
;--------------------------------------------------------------
edit_exit: 
         lda e_column 
         tay  
         lda temp
         sta (e_lo),y
         lda #$00
         sta temp  
         lda #$80
         sta e_hi
         lda #$cb
         sta e_lo 
         ldy #$00
         tya
         tax 
         lda s_lo  
         pha
         lda s_hi  
         pha
--       ldy #$00 
         sty temp 
-        lda (e_lo),y
         cmp #$a0
         bne +
         lda temp
         ora tab,y
         sta temp 
+        iny
         cpy #$08  
         bne -         
         ldy #$00
         lda temp
         sta (s_lo),y 
         inc s_lo    
         clc
         lda e_lo
         adc #$28
         sta e_lo
         bcc ++
         inc e_hi 
++       inx
         cpx #$08
         bne --
         pla
         sta s_hi   
         pla
         sta s_lo   
         jmp keys
tab: !by $80,$40,$20,$10,$08,$04,$02,$01
;--------------------------------------------------------------
; edit bit in edit-window
;--------------------------------------------------------------
edit_toggle:
         lda temp
         cmp #$a0
         bne +
         and #$7f
         sta temp
         jmp edit_keys 
+        ora #$80 
         sta temp
         jmp edit_keys 
;--------------------------------------------------------------
; edit char entry
;--------------------------------------------------------------
edit_char:
         ldy #$00 
         sty e_column  
         sty e_row
         lda #$80
         sta e_hi
         lda #$cb
         sta e_lo 
         lda (e_lo),y 
         sta temp
         lda #"*"
         sta (e_lo),y 
edit_keys:         
         jsr $ffe4
         cmp #"6"
         beq edit_right
         cmp #"4"
         beq edit_left
         cmp #"2"
         beq edit_down
         cmp #"8"
         beq edit_up
         cmp #"E"
         beq j_edit_exit       
         cmp #$20
         beq edit_toggle       
         jmp edit_keys
j_edit_exit: jmp edit_exit  
;--------------------------------------------------------------
; move right in editor-window
;--------------------------------------------------------------
edit_right:
         lda e_column
         cmp #$07
         bne +  
         beq edit_keys
+        lda e_column 
         tay  
         lda temp
         sta (e_lo),y
         iny
         lda (e_lo),y
         sta temp 
         lda #"*"
         sta (e_lo),y 
         inc e_column
         jmp edit_keys    
;--------------------------------------------------------------
; move left in editor-window
;--------------------------------------------------------------
edit_left:
         lda e_column
         beq edit_keys
         lda e_column
         tay    
         lda temp   
         sta (e_lo),y
         dey
         lda (e_lo),y
         sta temp 
         lda #"*"
         sta (e_lo),y 
         dec e_column
e:       jmp edit_keys    
;--------------------------------------------------------------
; move down in editor-window
;--------------------------------------------------------------
edit_down:
         lda e_row
         cmp #$07
         beq e      
         lda e_column
         tay    
         lda temp
         sta (e_lo),y   
         clc
         lda e_lo
         adc #$28
         sta e_lo
         bcc +
         inc e_hi
+        lda (e_lo),y  
         sta temp 
         lda #"*"
         sta (e_lo),y 
         inc e_row
         jmp edit_keys       
;--------------------------------------------------------------
; move up in editor-window
;--------------------------------------------------------------
edit_up:
         lda e_row
         beq e      
         lda e_column
         tay    
         lda temp
         sta (e_lo),y   
         sec
         lda e_lo
         sbc #$28
         sta e_lo
         bcs +
         dec e_hi  
+        lda (e_lo),y  
         sta temp 
         lda #"*"
         sta (e_lo),y 
         dec e_row
         jmp edit_keys       
;--------------------------------------------------------------
; Filehandling Basic 4: load charset
;--------------------------------------------------------------
charset_load:      
         lda #$00
         sta status
         lda #$0f
         sta la 
         ldx #$08
         stx fa 
         ldy #$60
         sty sa
         jsr open
         jsr listen
         lda #$ff    ; Load to $1500
         sta $fb
         ldx #$14
         stx $fc 
; BASIC 4 ROM-Patch to skip load-addr from file
         ldy fnlen		  
     	 jsr $f4a5   ; Get Next One Byte Parameter
         jsr talk
         lda sa      ; Current Secondary Address
  	 jsr $f193   ; set   
         jsr unlisn   
         jsr $f38c   ; Load jump in
         lda #$0f 
         jsr close
         rts
filename: !by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;--------------------------------------------------------------
; Filehandling Basic 4 : save charset
;--------------------------------------------------------------
charset_save:      
         lda #$00
         sta $fb
         lda #$15
         sta $fc
         lda #$ff
         sta $c9
         lda #$16
         sta $ca
         lda #$00
         sta status
         lda #$0f
         sta la 
         ldx #$08
         stx fa 
         ldy #$60
         sty sa  
         lda #$00
         sta $fb
         lda #$15
         sta $fc
         lda #$ff
         sta $c9
         lda #$16
         sta $ca
         jsr open
         jsr talk
         jsr save
         lda #$0f 
         jsr close
         rts 
;--------------------------------------------------------------
; Filehandling Basic 4 : directory
;--------------------------------------------------------------
directory:
          ldy #$00 
-         lda $8000,y
          sta $1000,y 
          lda $8100,y
          sta $1100,y 
          lda $8200,y
          sta $1200,y 
          lda $8300,y
          sta $1300,y 
          iny
          bne -
          lda #$93
          jsr bsout    
          jsr $d873  
-         jsr $ffe4
          cmp #$20
          beq +
          bne -
+         ldy #$00 
-         lda $1000,y 
          sta $8000,y
          lda $1100,y 
          sta $8100,y
          lda $1200,y 
          sta $8200,y
          lda $1300,y 
          sta $8300,y
          iny
          bne -
          jmp keys 
screen:
!scr "charedit for pet pcg           (BASIC 4)"
!scr "----------------------------------------"
!scr "                                        "
!scr " ############                           "
!scr " #          #     [w]rite charset to pcg"
!scr " #          #     [e]dit char           "
!scr " #          #       4/6 left/right      "
!scr " #          #       8/2 up/down         "
!scr " #          #       space to edit       "
!scr " #          #                           "
!scr " #          #     [d]irectory           "
!scr " #          #     [l]oad                "
!scr " #          #     [s]ave                "
!scr " #          #     [q]uit                "
!scr " ############                           "
!scr "                                        "
!scr "                                        "
!scr " -------------------------------------- "
!scr " charset                                "
!scr "        "
;*=$1500
;space for the charset, up to $16ff





