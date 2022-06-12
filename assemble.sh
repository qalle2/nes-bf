# Warning: this script DELETES files. Run at your own risk.
asm6 bf.asm bf.nes
rm -f *.gz
gzip -k9 *.nes
