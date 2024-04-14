!to"charedit",cbm
; lots of offsets and variables
s_lo   = $0a
s_hi   = $0b
t_lo   = $0c
t_hi   = $0d
temp   = $10
lines  = $11
column = $12
row    = $13
c_lo   = $14
c_hi   = $15
e_lo   = $16
e_hi   = $17
e_column =$18
e_row =$19

; file handling
fnadr        = $da
fnlen        = $d1
status       = $96
sa           = $d3 
fa           = $d4
open         = $f466
talk         = $f0b6
basin        = $ffcf
secnd        = $f128
load         = $f355
save         = $f6a4
close        = $f2ac  

; screen and char
screen_ram = $8000
char = $1500

cur_col =  $c6
cur_row =  $d8
setcur  = $e25d
bsout   = $ffd2

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

          lda #<filename 
          sta fnadr   
          lda #>filename 
          sta fnadr+1

          lda #$12
          sta cur_row
          lda #$0a
          sta cur_col    
          jsr setcur 

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
         lda #$ca
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
         ldy #$08  
         lda temp 
         and #0b00000001
         beq b1
         lda #$a0
         sta (t_lo),y
         jmp +   
b1:      lda #$20
         sta (t_lo),y
+        dey 
         lda temp                 
         and #0b00000010
         beq b2
         lda #$a0
         sta (t_lo),y
         jmp + 
b2:      lda #$20
         sta (t_lo),y
+        dey
         lda temp                 
         and #0b00000100
         beq b3
         lda #$a0
         sta (t_lo),y
         jmp +  
b3:      lda #$20
         sta (t_lo),y
+        dey 
         lda temp                 
         and #0b00001000
         beq b4
         lda #$a0
         sta (t_lo),y
         jmp +
b4:      lda #$20
         sta (t_lo),y
+        dey 
         lda temp                 
         and #0b00010000
         beq b5
         lda #$a0
         sta (t_lo),y
         jmp +
b5:      lda #$20
         sta (t_lo),y
+        dey 
         lda temp                 
         and #0b00100000
         beq b6
         lda #$a0
         sta (t_lo),y
         jmp +  
b6:      lda #$20
         sta (t_lo),y
+        dey 
         lda temp                 
         and #0b01000000
         beq b7
         lda #$a0
         sta (t_lo),y
         jmp +    
b7:      lda #$20
         sta (t_lo),y
+        dey 
         lda temp                 
         and #0b10000000
         beq +
         lda #$a0
         sta (t_lo),y
         jmp end
+        lda #$20
         sta (t_lo),y
end:     rts
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
         ora ortab,y
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
ortab: !by $80,$40,$20,$10,$08,$04,$02,$01
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
;--------------------------------------------------------------
; keys for edit  
;--------------------------------------------------------------
j_edit_exit: jmp edit_exit  
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
+        
         lda (e_lo),y  
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
; Filehandling Basic 2 : load charset
;--------------------------------------------------------------
charset_load:      
          lda #$08
          sta $fa             ; device
          lda #$00
          sta status             
          lda #$08 
          sta fa              ; secaddr
          lda #$60
          sta sa              ; priaddr
          jsr open            ; open
          jsr talk            ; talk
          lda sa
          jsr secnd
          lda #$fe            ; load to $1500 
          sta $fb
          lda #$14
          sta $fc 
          jsr load              
          rts
filename: !by $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;--------------------------------------------------------------
; Filehandling Basic 2 : save charset
;--------------------------------------------------------------
charset_save:      
          lda #$00
          sta status             
          lda #$08
          sta $fa             ; device
          lda #$60
          sta sa              ; priaddr
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
          rts 
screen:
!scr "charedit for pet pcg                    "
!scr "----------------------------------------"
!scr "                                        "
!scr " ############                           "
!scr " #          #     [w]rite charset to pcg"
!scr " #          #     [e]dit char           "
!scr " #          #       4/6 left/right      "
!scr " #          #       8/2 up/down         "
!scr " #          #       space to edit       "
!scr " #          #                           "
!scr " #          #                           "
!scr " #          #     [l]oad                "
!scr " #          #     [s]ave                "
!scr " #          #     [q]uit                "
!scr " ############                           "
!scr "                                        "
!scr "                                        "
!scr " -------------------------------------- "
!scr " charset                                "
!scr "        "
*=$1500
;space for the charset, up to $16ff





