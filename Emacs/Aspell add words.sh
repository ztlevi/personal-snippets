touch ~/aspell.personal.txt
vim ~/aspell.personal.txt # add words per line
aspell --lang=en create master /tmp/en-personal.pws < ~/aspell.personal.txt
cp /tmp/en-personal.pws /usr/lib/aspell
vim /usr/lib/aspell/en_US.multi # add the line: 'add en-personal.pws'