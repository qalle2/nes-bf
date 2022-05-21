# Warning: this script DELETES files. Run at your own risk.
rm -f *.gz *.nes
asm6 bf.asm bf.nes
gzip -k --best bf.nes
