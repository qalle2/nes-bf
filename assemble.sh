asm6 bf.asm bf.nes
tar -cf movies.tar *.fm2
gzip -9fk *.bin *.nes
gzip -9f *.tar
