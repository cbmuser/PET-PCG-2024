# PET-PCG-2024
In the early 80th HAL Laboratory in Japan created a programmable character set generator and sound device for PET/CBM computers. This used the userport and the charater-socket for connection. So it was very easy to use with the first BASIC 1 machines. 
Mike Naberenzy reversed the logic and reassembled the software. A big thanks for that !  
http://mikenaberezny.com/hardware/pet-cbm/hal-pcg-6500-graphics-board/

It's also possible to rebuild the original hardware: https://github.com/InsaneDruid/PCG-6500 for the early BASIC1-machines. There you'll find also other usefull stuff for your PET.


My version fits into the newer dynamic PET-board (CBM 2001/3001) and uses DMA-access at **$a000** or $9000 for transfering the chars. There is no CB2-sound, I want to combine this in the future with a joystick-adapter at the userport.
![PCB](https://github.com/cbmuser/PET-PCG-2024/blob/main/pcg2024_PCB.jpg)
You can use a IDT7130 in DIP or PLCC52. **You only need one of them !**  
The actual hardware is not tested in all cases (on/off function). Basically it works and new software is under construction, also some games from the HAL-adapter will be patched to this version. The board-routing was done in development-progress, so it works, but it's not the best it can be.  

**This is open hard- and software. Use it on your own risc. There is no warranty at all !**

![development](https://github.com/cbmuser/PET-PCG-2024/blob/main/pet_pcg2024.jpg)

The first board for development. The first idea was to use banking for addressing the whole ram and the bigger IDT7132. I've kicked this because no one will ever write software for that.

![development1](https://github.com/cbmuser/PET-PCG-2024/blob/main/pet_pcg_flachband.jpg)

The char-rom and the connection to the socket fits on the solder-side.



A first character-editor 

![charedit](https://github.com/cbmuser/PET-PCG-2024/assets/34414160/4d7a54f9-4bdf-4e4e-bc9e-624cf71814df)

It uses $a000 for the DMA. If you want to compile it for $9000 use the ACME Cross-Assembler. https://sourceforge.net/projects/acme-crossass/ 

