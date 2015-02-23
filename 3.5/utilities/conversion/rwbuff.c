#include <stdio.h>

/*  @(#)rwbuff.c	5.1 06/22/94   */

readb_(num_read, try_read, buffer, char_len)

char *buffer;
int *try_read, *num_read, char_len;

{
      int try_to_read;

      try_to_read = *try_read;
      *num_read = read(0,buffer,try_to_read);

} 

int writeb_(num_write, try_write, buffer, char_len)

char*buffer;
int *try_write, *num_write, char_len;

{
  	int try_to_write;

	try_to_write = *try_write;
	*num_write = write(1,buffer,try_to_write);

}

