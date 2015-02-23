/* @(#)shm_graph.c	5.2 07/08/94  
* Shared memory routines for SunCore/SunView graphics
*/

# include <stdio.h>
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <sys/sem.h>

# define ARRAYSIZE 4096
# define NUMCHAR 80
# define BYTES 376836

/* NUMCHAR = number of characters to be sent for CHAR graphics command
*  ARRAYSIZE = number of graphics commands to send before doing a FLUSH (larger
*              the better but not TOO much that huge amounts of memory are used)
*  BYTES = number of bytes in shared memory = 
*	   ARRAYSIZE*(NUMCHAR + 12) + 4
*/ 

/* ---------------------------------------------------------------
* The following was lifted from IPC_MAC.H, created by Allen Farris and is
* an easy-to-use set of shared memory functions
*
*
* Define a key to be used for shared memory 
* Define the shared memory id -- only one shared memory region allowed
* Define errno
*/

static key_t shm_key;
static int shm_id;
extern int errno;

/* Create shared memory */

# define create_shm(SHMKEY,BYTES,RWFLAG) \
	strncpy(((char *)&shm_key),SHMKEY,4); \
 	shm_id = shmget(shm_key,BYTES,IPC_CREAT | RWFLAG); \
	if (shm_id == -1) \
	{ \
		printf("Failure creating shared memory (%d)\n",errno); \
		exit(-1); \
	}

/* Access shared memory that has already been created */

# define access_shm(SHMKEY,BYTES,RWFLAG) \
	strncpy(((char *)&shm_key),SHMKEY,4); \
	shm_id = shmget(shm_key,BYTES,RWFLAG); \
	if (shm_id == -1) \
	{ \
		printf("Failure acessing shared memory (%d)\n",errno); \
		exit(-1); \
	}

/* Attach shared memory -- returns address of shared memory or -1 */

# define attach_shm(SHMADDR) \
	shmat(shm_id,0,0)

/* Deallocate shared memory */

struct shmid_ds shmem;
# define destroy_shm() \
	if (shmctl(shm_id, IPC_STAT, &shmem) == -1) \
	{ \
		printf("Could not deallocate shared memory\n"); \
		exit(-1); \
	} \
	if (shmctl(shm_id, IPC_RMID, &shmem) == -1) \
	{ \
		printf("Could not deallocate shared memory\n"); \
		exit(-1); \
	}

/*  That is it for Allen's stuff -- Now for my creations 
* ---------------------------------------------------------------
*/

/* The shared memory structure used by UniPOPS and the Graphiuucs process 
* FLAG = the number of elements used in shared memory
* COMMAND = the graphics commands 1=PLACE, 2=VCTR, 10=CHAR
* POS1, POS2 = X,Y plot positions for PLACE and VCTR
* BUFFER = character string for CHAR
*/

typedef struct shared_memory_structure
{
        long int flag;
	long int command[ARRAYSIZE];
	long int pos1[ARRAYSIZE];
	long int pos2[ARRAYSIZE];
	char buffer[ARRAYSIZE][NUMCHAR];
} ShmGraph;


ShmGraph *shm;

/* R/WNEXT_LOC = the location in shared memory where to read or write next */

long int rnext_loc, wnext_loc;


createshm2_(memname, len)

/* Creates Shared memory -- Called by UniPOPS */

long int len;
char *memname;

{

	create_shm(memname,BYTES,IPC_CREAT | 0644);
	shm = (ShmGraph *) (attach_shm(0));
        if (shm == (ShmGraph *)(-1)) {
           printf("Failure attaching shared memory (%d)\n",errno);
           exit(-1);
        }
	wnext_loc = 0;

}

writeshm2_(cmnd, pos1, pos2, string, len)

/* Writes graphics commands and strings to shared memory -- Used by UniPOPS */

char *string;
long int len, *cmnd, *pos1, *pos2;

{
	wnext_loc = shm->flag;
        shm->command[wnext_loc] = *cmnd;
	shm->pos1[wnext_loc] = *pos1;
	shm->pos2[wnext_loc] = *pos2;
	if (*cmnd >= 10) strncpy(shm->buffer[wnext_loc], string, NUMCHAR);
	shm->flag = wnext_loc + 1;
/*      Update shared memory and bump the WNEXT_LOC pointer */

	if (shm->flag >= ARRAYSIZE) coreflush_();
/*	Tell the Graphics process to start using the contents of shared memory
*	Wait till the process uses all of the contents of shared memory
*/
}

destroyshm2_()

/* Destroys the shared memory -- Called by UniPOPS */

{
	destroy_shm();

}

accessshm_(memname, len)

/* Access shared memory -- Used by Graphics process and not UniPOPS */

char *memname;
long int len;

{
	access_shm(memname, BYTES, 0644);
	shm = (ShmGraph *) (attach_shm(0));
        if (shm == (ShmGraph *)(-1)) {
           printf("Failure attaching shared memory (%d)\n",errno);
           exit(-1);
        }
	rnext_loc = 0;

}

long int readshm2_(cmnd, pos1, pos2, string, len)

/* Read shared memory -- Used by Graphics process and not UniPOPS */

char *string;
long int *cmnd, *pos1, *pos2, len;

{
	if (rnext_loc >= shm->flag) 
	{
	    rnext_loc = 0;
	    shm->flag = 0;
	}
/* Checks whether the full contents of the shared memory arrays have been used */
	else
	{
	    *cmnd = shm->command[rnext_loc];
	    *pos1 = shm->pos1[rnext_loc];
	    *pos2 = shm->pos2[rnext_loc];
	    if (*cmnd >= 10) strncpy(string, shm->buffer[rnext_loc], NUMCHAR);
	    rnext_loc++;
	}
/* Gets the commands, positions, strings from shared memory -- bump the 
* read pointer
*/

	return(shm->flag);

}

long int getnextloc_()

/* Finds out if there is anything in shared memory -- Used by UniPOPS */

{
	return(shm->flag);
}

resetshm_()

/* Clears the flags in shm  */

{
	shm->flag = 0;
}

/*   Reads and writes to named pipes */

#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

readfifo_(id, irtn, string, len)

char *string;
long int len, *irtn, *id;

{
	char input;
	int i;

	for (i = 0; i < len; i++)  {
	   *irtn = read(*id, &input, 1);
	   if (*irtn < 0) perror("readfifo");
	   if (input == '\n' || *irtn < 0) break;
	   string[i] = input;
	}
}

writefifo_(id, irtn, string, len)

char *string;
long int len, *irtn, *id;

{

	*irtn = write(*id,string,len);
	if (*irtn < 0) perror("writefifo");
	*irtn = write(*id,"\n",1);
	if (*irtn < 0) perror("writefifo");

}

openread_(irtn, string, len)

char *string;
long int len, *irtn;

{
	*irtn = open(string, O_RDONLY);
	if (*irtn < 0) perror("openread");
}


openwrite_(irtn, string, len)

char *string;
long int len, *irtn;

{
	*irtn = open(string, O_WRONLY);
	if (*irtn < 0) perror("openwrite");
}
closefifo_(ifd, irtn)

long int *irtn, *ifd;

{
	*irtn = close(*ifd);
	if (*irtn < 0) perror("closewrite (close)");
}
