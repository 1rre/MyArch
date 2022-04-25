#ifndef header_micro_malloc
#define header_micro_malloc

#define MALLOC_BLOCK_SIZE 128
typedef unsigned char byte;

void* mmalloc(int);
void* mrealloc(void*, int);
void mmset(void*, byte, int);
void mfree(void*);
void mmcpy(void*, int, void*);

#endif /* header_micro_malloc */