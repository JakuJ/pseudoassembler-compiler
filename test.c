#include <stdlib.h>
#include <stdio.h>
int main(void)
{
int * memory = (int*)calloc(1000, sizeof(int));
int * registers = (int*)calloc(16, sizeof(int));
registers[14] = 1000; registers[15] = 2000;
int flag = 0;
#define TAB 0
for(int i = 0; i < 1; i++){memory[i + 0] = 0;}
#define TABA 1
for(int i = 0; i < 1; i++){memory[i + 1] = 1;}
#define TABB 2
for(int i = 0; i < 1; i++){memory[i + 2] = 1;}
#define TABC 3
for(int i = 0; i < 1; i++){memory[i + 3] = 7;}
#define TABD 4
for(int i = 0; i < 1; i++){memory[i + 4] = 3;}
#define TABE 5
for(int i = 0; i < 1; i++){memory[i + 5] = 5;}
#define TABF 6
for(int i = 0; i < 1; i++){memory[i + 6] = 8;}
#define TABG 7
for(int i = 0; i < 1; i++){memory[i + 7] = 14;}
#define TABH 8
for(int i = 0; i < 1; i++){memory[i + 8] = 21;}
#define TABI 9
for(int i = 0; i < 1; i++){memory[i + 9] = 34;}
#define ZERO 10
for(int i = 0; i < 1; i++){memory[i + 10] = 0;}
#define JEDEN 11
for(int i = 0; i < 1; i++){memory[i + 11] = 1;}
#define CZTERY 12
for(int i = 0; i < 1; i++){memory[i + 12] = 4;}
#define WYNIK 13
registers[1] = memory[ZERO]; // L
registers[2] = memory[JEDEN]; // L
registers[4] = TAB * 4 + 1000; // LA
registers[5] = ZERO * 4 + 1000; // LA
PETLA:;
flag = registers[1] - memory[(registers[4] - 1000 + 0) / 4]; // C
if (flag > 0) {goto NIE;} // JP 
if (flag < 0) {goto NIE;} // JN 
registers[4] += memory[CZTERY]; // A
flag = registers[4] - registers[5]; // CR
if (flag == 0) {goto TAK;} // JZ 
registers[3] = memory[ZERO]; // L
registers[3] += registers[1]; // AR
registers[3] += registers[2]; // AR
registers[1] = registers[2]; // LR
registers[2] = registers[3]; // LR
goto PETLA; // J
NIE:;
registers[4] = memory[ZERO]; // L
memory[WYNIK] = registers[4]; // ST
goto KONIEC; // J
TAK:;
registers[4] = memory[JEDEN]; // L
memory[WYNIK] = registers[4]; // ST
KONIEC:;

printf("REGISTER DUMP:\n");
for(int i = 0; i < 16; i++){printf("%d:\t%d\n", i, registers[i]);}
printf("MEMORY DUMP:\n");
for(int i = 0; i < 1000; i++){if (memory[i] != 0) {printf("%d:\t%d\n", 1000 + 4 * i, memory[i]);}}
return 0;
}
