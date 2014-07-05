/***************************************************************************** 
* GROM Version 0.75                                                          * 
* Bart Trzynadlowski, 2000, public domain.                                   * 
*****************************************************************************/ 
 
/* 
 * grom.c:  
 * 
 * Simple utility for toying with Genesis ROMs. Supports converting of SMD to 
 * BIN ROM formats. Also displays information from the Sega cartridge header. 
 * 
 * Much thanks to Kuwanger (http://members.tripod.com/~Kuwanger), XnaK, and 
 * Later Dude. 
 */ 
 
/* for DOS: make sure to do a #define DOS here */ 
 
#include <stdio.h> 
#include <string.h> 
 
#define VERSION "0.75" 
#define error(str1, str2)					\
	{							\
		printf(str1, str2);				\
		return 1;					\
	}
#define lower(str, i, j)					\
	{							\
		for (i = 0; str[i] != '\0'; i++)		\
			for (j = 0; j < 6; j++)			\
				if (str[i] == ltab[j])		\
				{				\
					str[i] = ltab[j + 6];	\
					break;			\
				}				\
	}
 
/* functions */ 
int convert_smd_bin(char *); 
int show_info(char *); 
void smd_bin(unsigned char *, unsigned char *); 
int check_format(FILE *, char *, char *); 
 
/* environment */ 
int nochk = 0; 
 
/* letter conversion table */
char ltab[] = { 'B', 'I', 'N', 'S', 'M', 'D', 'b', 'i', 'n', 's', 'm', 'd' }; 
 
/***************************************************************************** 
 int main(int, char **): 
                Stop reading this source code if you don't know what main() 
                is. Your head might pop. 
 Input:         Obvious. 
 Returns:       Obvious. 
*****************************************************************************/ 
int main(int argc, char **argv) 
{ 
        int i, j, f = 0; 
#ifdef DOS 
        int k; 
#endif 
 
        /* show help */ 
        if (argc <= 1) 
        { 
                printf("GROM Version %s by Bart Trzynadlowski, 2000\n", 
                       VERSION); 
                printf("A Free Multi-function Sega Genesis ROM Utility\n"); 
                printf("\n"); 
                printf("Usage:    grom [files] [commands] [options]\n"); 
#ifdef DOS 
                printf("File:     Genesis ROM file, BIN or SMD format\n"); 
#endif 
#ifndef DOS 
                printf("File:     Genesis ROM file, BIN or SMD format. " \
                       "Must have lowercase extension\n"); 
#endif 
                printf("Commands: -bin    Convert file(s) to BIN format\n"); 
                printf("          -info   Show information about file(s)\n"); 
                printf("Options:  -chk    Enable detailed ROM checks\n"); 
                printf("          -nochk  Disable detailed ROM checks\n"); 
                return 0; 
        } 
        else if (argc <= 2)     /* user didn't specify an option */ 
        { 
                printf("main: Not enough arguments\n"); 
                return 0; 
        } 
 
        /* find options, don't remove them from argv[] */ 
        for (i = 1; i < argc; i++)    
                if (strcmp(argv[i], "-chk") == 0) 
                        nochk = 0; 
                else if (strcmp(argv[i], "-nochk") == 0) 
                        nochk = 1;                 
 
#ifdef DOS 
        /* go through the command line, convert letters s, m, d, and b, i, n 
           to lowercase to prevent problems with strstr() later on */ 
        for (i = 1; i < argc; i++) 
	{
                lower(argv[i], j, k); 
	}
#endif 
 
        /* actually process commands, as each one is processed, we remove it 
           from the argv[] array. */ 
        for (i = 1; i < argc; i++) 
        { 
                if (strcmp(argv[i], "-bin") == 0)       /* SMD to BIN */ 
                { 
                        f = 1; 
 
                        /* convert each file to bin */ 
                        for (j = 1; j < argc; j++) 
                        { 
                                if (*argv[j] == '-')    /* - means option */ 
                                        ; 
                                else 
                                        convert_smd_bin(argv[j]); 
                        } 
                } 
                else if (strcmp(argv[i], "-info") == 0) /* show info */ 
                { 
                        f = 1; 
 
                        /* print information on each file */ 
                        for (j = 1; j < argc; j++) 
                        { 
                                if (*argv[j] == '-') 
                                        ; 
                                else 
                                        show_info(argv[j]); 
                        } 
                } 
        } 
 
        /* no options found */ 
        if (!f) 
                printf("main: No commands specified\n"); 
 
        return 0; 
} 
 
/***************************************************************************** 
 int convert_smd_bin(char *): 
                Converts an SMD ROM file (<filename>.smd) to a BIN ROM file 
                (<filename>.bin). 
 Input:         File path. 
 Returns:       Nothing. 
*****************************************************************************/ 
int convert_smd_bin(char *file) 
{ 
        int i, num_blocks, meter = 0; 
        FILE *in_fp, *out_fp; 
        unsigned char smd_block[16384]; 
        unsigned char bin_block[16384]; 
 
        /* open SMD file, open BIN file, get number of blocks, read header */ 
        if ((in_fp = fopen(file, "rb")) == NULL) 
                error("convert_smd_bin: SMD file could not be opened: %s\n", 
                      file); 
        if (check_format(in_fp, file, "smd"))   /* check if it is SMD */ 
                return 1; 
        if ((out_fp = fopen(strcat(strtok(file, "."), ".bin"), "wb")) == NULL) 
                error("convert_smd_bin: BIN file could not be opened: %s\n", 
                       file); 
        fseek(in_fp, 0, SEEK_END); 
        fgetpos(in_fp, (void *) &num_blocks); 
        num_blocks = ((num_blocks + 1) - 512) / 16384; 
        fseek(in_fp, 512, SEEK_SET); 
        
        /* turn off line buffering on stdout so we can see progress meter */ 
        setvbuf(stdout, NULL, _IONBF, NULL); 
 
        printf("convert_smd_bin: Converting %s to BIN format", 
               strcat(strtok(file, "."), ".smd")); 
 
        /* convert smd to bin */ 
        for (i = 0; i < num_blocks; i++) 
        { 
                fread(smd_block, 1, 16384, in_fp); 
                smd_bin(bin_block, smd_block); 
                fwrite(bin_block, 1, 16384, out_fp); 
 
                /* increase meter every 1/10th total blocks */ 
                meter++; 
                if (meter == (num_blocks / 10)) 
                { 
                        printf("."); 
                        meter = 0; 
                } 
        } 
 
        printf(" done\n"); 
 
        setvbuf(stdout, NULL, _IOLBF, NULL);    /* line buffering back on */ 
 
        /* close files and exit */ 
        fclose(in_fp); 
        fclose(out_fp); 
 
        return 0; 
} 
 
/***************************************************************************** 
 int show_info(char *file): 
                Shows information about a given file. If it is a binary file 
                information will be obtained from the Sega header, otherwise 
                we will show whatever information we can obtain. 
 Input:         File path. 
 Returns:       Nothing. 
*****************************************************************************/ 
int show_info(char *file) 
{ 
        FILE *fp; 
        unsigned char header[16384]; 
        unsigned char smd[16384]; 
        char string[49]; 
 
        /* check to see if it a BIN or SMD file */ 
        if ((strstr(file, ".bin") != NULL) || (strstr(file, ".smd") != NULL)) 
        { 
                /* open file and read the header */ 
                if ((fp = fopen(file, "rb")) == NULL) 
                        error("show_info: File could not be opened: %s\n", 
                              file); 
 
                if (strstr(file, ".bin") != NULL)       /* bin file */ 
                        fread(header, 1, 512, fp); 
                else                                    /* smd file */ 
                { 
                        /* get first block of SMD */ 
                        fread(smd, 1, 512, fp);         /* skip header */ 
                        fread(smd, 1, 16384, fp); 
                        smd_bin(header, smd);           /* decode */ 
                } 
 
                printf("Showing information from cartridge data...\n"); 
 
                /* print info from Sega header */ 
                printf("                    File: %s\n", file); 
                string[16] = '\0'; 
                strncpy(string, &header[0x100], 16); 
                printf("                  System: %s\n", string); 
                strncpy(string, &header[0x110], 16); 
                printf("               Copyright: %s\n", string); 
                string[48] = '\0'; 
                strncpy(string, &header[0x120], 48); 
                printf("    Game name (domestic): %s\n", string); 
                strncpy(string, &header[0x150], 48); 
                printf("    Game name (overseas): %s\n", string); 
                if (header[0x180] == 'G' && header[0x181] == 'M') 
                        printf("           Software type: game\n"); 
                else if (header[0x180] == 'A' && header[0x181] == 'l') 
                        printf("           Software type: educational\n"); 
                else 
                        printf("           Software type: %c%c", 
                               header[0x180], header[0x181]); 
                /* from personal observation, it seems the product code 
                   field starts at 0x183, and is 11 bytes long. 0x182 may be 
                   a continuation of the software type field, but I am most 
                   likely wrong */ 
                string[11] = '\0'; 
                strncpy(string, &header[0x183], 11); 
                printf("Product code and version: %s\n", string); 
                printf("                Checksum: %02X%02X\n", header[0x18e], 
                       header[0x18f]); 
                string[16] = '\0'; 
                strncpy(string, &header[0x190], 16); 
                printf("             I/O support: %s\n", string); 
                /* the meaning of these fields may have been misinterpreted */ 
                printf("       ROM start address: %02X%02X%02X%02X\n", 
                       header[0x1a0], header[0x1a1], header[0x1a2], 
                       header[0x1a3]); 
                printf("         ROM end address: %02X%02X%02X%02X\n", 
                       header[0x1a4], header[0x1a5], header[0x1a6], 
                       header[0x1a7]); 
                /* is the modem data field really 20 bytes? XnaK's document 
                   seems to indicate it is only 10... */ 
                string[20] = '\0'; 
                strncpy(string, &header[0x1bc], 20);                 
                printf("              Modem data: %s\n", string); 
                string[40] = '\0'; 
                strncpy(string, &header[0x1c8], 40); 
                printf("                    Memo: %s\n", string); 
                string[3] = '\0'; 
                strncpy(string, &header[0x1f0], 3); 
                printf("               Countries: %s\n", string); 
 
                fclose(fp); 
        } 
        else 
                printf("show_info: Unrecognized file format: %s\n", file); 
 
        return 0; 
} 
 
/***************************************************************************** 
 void smd_bin(unsigned char *, unsigned char *): 
                Converts a 16KB SMD block to BIN. 
 Input:         Destination buffer address, source buffer address. 
 Returns:       Nothing. 
*****************************************************************************/ 
void smd_bin(unsigned char *bin_block, unsigned char *smd_block) 
{ 
        int i, o = 1, e = 0; 
 
        /* convert 16KB of SMD to BIN */ 
        for (i = 0; i < 8192; i++) 
        { 
                bin_block[o] = smd_block[i];                 
                bin_block[e] = smd_block[i+8192]; 
                o += 2; 
                e += 2; 
        } 
}         
 
/***************************************************************************** 
 int check_format(FILE *, char *, char *): 
                Checks the given file to make sure it is of a given ROM type. 
 Input:         File pointer, file name, a string describing the format: "bin" 
                or "smd". 
 Returns:       0 if okay, 1 if not. 
 Note:          Assumes file is at SEEK_SET. Will return with file at SEEK_SET 
                if check is successful, otherwise the pointer will be at 512.                 
*****************************************************************************/ 
int check_format(FILE *fp, char *file, char *type) 
{ 
        unsigned char buffer[512]; 
 
        if (nochk)      /* don't perform checks if user doesn't want to */ 
                return 0; 
 
        if (strcmp(type, "bin") == 0)           /* BIN */ 
        { 
                if (strstr(file, ".bin") == NULL)       /* check extension */ 
                        error("check_format: File %s may not be in BIN " \
                              "format, operation cancelled\n", file); 
                fread(buffer, 1, 512, fp); 
                /* check for "SEGA" text */ 
                if (strncmp(buffer + 0x100, "SEGA", 4) != 0) 
                        error("check_format: File %s may not be in BIN " \
                               "format, operation cancelled\n", file);
        } 
        else if (strcmp(type, "smd") == 0)      /* SMD */  
        { 
                if (strstr(file, ".smd") == NULL) 
                        error("check_format: File %s may not be in SMD " \
                              "format, operation cancelled\n", file); 
                fread(buffer, 1, 512, fp); 
                /* check for 0xaa at 8, 0xbb at 9 */ 
                if ((buffer[8] != 0xaa) || (buffer[9] != 0xbb)) 
                        error("check_format: File %s may not be in SMD " \
                              "format, operation cancelled\n", file); 
                /* if there is a "SEGA" text, it is probably in BIN format */ 
                if (strncmp(buffer + 0x100, "SEGA", 4) == 0) 
                        error("check_format: File %s may not be in SMD " \
                              "format, operation cancelled\n", file); 
        } 
 
        rewind(fp); 
        return 0; 
} 
 
 
                                 
                          
 
 

