;======================================================================================================
;CONWAYS GAME OF LIFE SIMULATOR
;======================================================================================================
IF1
	INCLUDE C:\Tasm\gamemac.mac
ENDIF
;======================================================================================================
DATA SEGMENT PARA PUBLIC 'data'
;map data
filename	DB		100 DUP(?)									;name of the file, read from keyboard
handle		DW		?											;handle of the file, used to read data from file
map			DW		16 DUP(0)									;contains the current state of the game
newmap		DW		16 DUP(0)									;contains the next state of the game
shift 		DW		?											;8000H - 4000H ... -used when reading to set the map according to the data
;simulation data
paused		DB		0											;running 0FFH ... paused 00H
duration	DW		?											;the duration of a single simulation
speed		DW		?											;the delay between two states
dimension1	DW		16											;dimensions of the board
dimension2	DW		16

;video memory data
startaddr		DW		0A000h									;start address of video memory

;screen data
screen_width DW 	320		
screen_height DW 	200								

;rectangle data
rect_width	DW		10											;size of the cells
rect_height	DW		10
rect_color	DB		1

;messages
MSG_0	DB		'CONWAYS GAME OF LIFE$'
MSG_1	DB		'Enter simulation time:$'
MSG_2	DB		'Enter simulation speed:$'
MSG_3	DB		0AH,0DH,'Error: invalid input',0AH,0DH,'$'
MSG_4	DB		0AH,0DH,'Error: input not in range',0AH,0DH,'$'
MSG_5	DB		0AH,0DH,'Error: input overflow', 0AH, 0DH,'$'
MSG_6	DB		0AH,0DH,'Do you want to simulate again?(y/n)', 0AH, 0DH,'$'
MSG_7	DB		'Enter file name:$'
MSG_8	DB		0AH,0DH,'File not exists', 0AH, 0DH,'$'
MSG_9	DB		0AH,0DH,'Map loaded successfully', 0AH, 0DH,'$'
MSG_10	DB		0AH,0DH,'Error in file data', 0AH, 0DH,'$'
MSG_11	DB		'Steps remaining:$'
MSG_12	DB		'Alive cells:$'
MSG_13	DB		'    $'
MSG_14	DB		'Press any key to continue$'


;auxiliary variables
ZECE1	DW		10
digit	DB		0		;stores the current digit(or character) when reading a number from keyboard
tst		DW		?		;tests if cell is alive or not
col		DB		?
top		DW		?		;upper row when counting neighbours
middle	DW		?		;middle row when counting neighbours
bottom	DW		?		;bottom row
location DW		?

;statistics
alive_cells DW	?
DATA ENDS
;======================================================================================================
STCK SEGMENT PARA PUBLIC 'stack'
DB		100 DUP('MY_STACK')
STCK ENDS
;======================================================================================================
;macro: 		delay between two states
;limitation:	time on 2 bytes(max 0FFFFH)
;TIME:		 	bigger time, bigger delay
delay MACRO TIME
	LOCAL	P1,P2
	PUSH	DX
	PUSH 	CX
	MOV		DX,TIME
P1:	MOV		CX,0FFFH
P2:	DEC		CX
	JNZ		P2
	DEC		DX		
	JNZ		P1
	POP		CX
	POP		DX
ENDM
;======================================================================================================
;macro:			write a number to screen
;limitation:	max 2bytes
;AX:			must load the number in AX before calling the macro
WRITE MACRO
LOCAL NEXT1, PRINT, PRINTARE
PUSH AX
PUSH BX
PUSH CX
PUSH DX
MOV DX, 0
MOV BX, 0 
MOV CX, 10
NEXT1:
	DIV ZECE1			;divide by 10(word) AX - quotient DX - remainder
	PUSH DX				;push remainder on stack , first remainder is the last digit , so it must be printed last
	MOV DX, 0H
	INC BX
	CMP AX, 0			;check if the number(quotient) reacher 0(no more digits)
	JZ PRINT			
LOOP NEXT1
PRINT:
	MOV CX, BX			;CX - number of digits of the initial number
PRINTARE:
	POP DX				;extract the remainders from the stack
	ADD DL, 48
	MOV AH, 02H			;print the digit to the screen
	INT 21H
LOOP PRINTARE
POP DX
POP CX
POP BX
POP AX
ENDM
;======================================================================================================
;macro:			read a string
;STRING:		string to be read
READ_STRING MACRO STRING
LOCAL NEXT
PUSH AX
PUSH SI
LEA SI, STRING		;load address to SI
NEXT:
MOV AH, 01H			;read character
INT 21H
CMP AL, 0DH			;check if enter was pressed
JE EXIT				;if enter => finished
MOV [SI], AL		;place character to corresponding location
INC SI
JMP NEXT
EXIT:
MOV BYTE PTR [SI], '$'		;place end of string($) to the end of our string
POP SI
POP AX
ENDM
;======================================================================================================
;macro: reset and clear screen
reset_default_screen MACRO
MOV		AH,00H	
MOV		AL,03H
INT		10H
ENDM
;======================================================================================================
;macro: print message to given row and column
;DX:	address of the string to print
print_string MACRO X,Y
set_cursor X,Y
MOV		AH,09H
INT		21H
ENDM
;======================================================================================================
;macro: set cursor to a specific row and column
;X:		column of the cursor
;Y: 	row of the cursor
set_cursor MACRO X,Y
PUSH	DX
MOV		DL,X		;screen column
MOV		DH,Y		;screen row
MOV		AH,2		;parameter to set cursor's position
MOV		BH,0		;page number
INT 	10H
POP		DX
ENDM
;======================================================================================================
CODE SEGMENT PARA PUBLIC 'code'
ASSUME CS:CODE, DS:DATA ,SS:STCK
MAIN PROC FAR
PUSH	DS
XOR		AX,AX
PUSH	AX
MOV		AX,DATA
MOV		DS,AX

START:
reset_default_screen

;open the file which contains the map of the game
CALL 	open_file

;read the map from the file
CALL	read_map

;read the duration
LEA		DX,MSG_1
MOV		BX,OFFSET duration
CALL	read_number

;read the speed
LEA		DX,MSG_2
MOV		BX,OFFSET speed
CALL 	read_number

;screen setup
MOV		AH,00H			;set screen to 320 x 200 pixels
MOV		AL,19
INT		10H	
			
MOV		ES,startaddr	;set ES to the start address of the video memory

LEA		DX,MSG_0		;print CONWAYS GAME OF LIFE
print_string 20,1
LEA		DX,MSG_11		;print Steps remaining:
print_string 20,3
LEA		DX,MSG_12		;print Alive cells:
print_string 20,4

MOV		paused,0FFh		;set paused (0FFH) - at first the game is paused and waits for the user to start the simulation
CALL 	draw_map		;the initial state is drawn

RUN:					;start of simulation
MOV		AH,01H			;check if key was pressed
INT		16H		
JZ 		NOT_PRESSED		;if zero flag not set, no key was pressed
MOV		AH,00H			;get the key
INT		16H
CMP		AL,'s' 			;check if 's' key was pressed
JNE 	NOT_PRESSED		;if 's' was not pressed, continue
NOT		paused			;change paused (00H - running , 0FFH - paused)
NOT_PRESSED:
CMP		paused,0		;check if paused
JNE		RUN				;if paused not equal 00H, jump to RUN(skip drawing the map)
CALL	draw_map		;draw the cells + the grid
CALL	update			;after drawing the map, the new state is created in function of the current state 
delay 	speed			;the current state is displayed, and a delay is applied for the user to see the result

LEA		DX,MSG_13		;clear the previous numbers 
print_string 36,3		;using whitespaces
LEA		DX,MSG_13
print_string 36,4

DEC		duration		;at each step, decrease the duration
set_cursor 36,3			;print number of steps remaining and alive cells
MOV		AX,duration
WRITE
set_cursor 36,4
MOV		AX,alive_cells
WRITE
CMP		duration,0
JE		END_RUN			;if duration reaches 0, the simulation is over
JMP		RUN				;otherwise, the simulation continues
END_RUN:

CALL 	draw_map		;draw the last state
LEA		DX,MSG_14		;print: press any key to continue
print_string 0,20	
FINAL_STATE:
MOV		AH,01H			;check if key was pressed
INT		16H
JZ		FINAL_STATE

reset_default_screen

;print ending message and ask to repeat the simulation
AGAIN:
MOV		AH,09H
MOV		DX,OFFSET MSG_6 ;print : Do you want to simulate again?(y/n)
INT		21H
MOV		AH,01H
INT 	21H
CMP		AL,79H		;check if 'y' was pressed
JNE		NOT_Y
JMP		START		;jump to the start of the program and begin a new simulation
NOT_Y:				
CMP		AL,6EH		;check ig 'n' was pressed
JE		END_PROGRAM ;if yes, jump to the end of the program and return
JMP		AGAIN		;else invalid input, ask for another key(y or n)

END_PROGRAM:
reset_default_screen
RET
MAIN ENDP

;======================================================================================================
;procedure: 	ask the user to enter a path to a file
;				if the file exists, its handle will be stored in handle
;				otherwise, an error message is shown and the user is asked for another path
;return a handle to the opened file
open_file PROC
JMP 	START_OPEN
RETRY:
LEA		DX,MSG_8		;print: File not exists
MOV		AH,09H
INT 	21H
START_OPEN:
LEA		DX,MSG_7		;print: Enter file name
MOV		AH,09H
INT		21H
READ_STRING filename	;reads the file path to filename
MOV		AH,3DH
MOV		AL,00H
LEA		DX,filename
INT		21H				;tries to open the file
JC		RETRY			;if carry flag is set, something went wrong, retry
MOV		handle,AX		;the interrupt returns in AX the handle of the file, which is stored later in handle
RET
open_file ENDP
;======================================================================================================
;procedure:		read map from the opened file(file handler in handle)
;if all data is correct: print: Map loaded successfully
;otherwise notify the user that the data is incorrect, but the simulation will start nonetheless
read_map PROC
MOV		SI,0
CONTINUE_READ_MAP:
MOV		shift, 8000H		;set the shift to 8000H(100...0 - first bit of the word)
CONTINUE_READ:
;read a byte
MOV		AH,3FH				;read
LEA		DX,digit			;into digit(DB)
MOV		CX,1				;one byte
MOV		BX,handle			;from file with handler handle
INT		21H
;check if 0 or 1 was read
CMP 	digit,'0'			;check if '0' was read
JE		DEAD_CELL			
CMP		digit,'1'			;check if '1' was read
JE		ALIVE_CELL
JMP		ERROR_FILE			;otherwise stop the reding and print and error message

ALIVE_CELL:
MOV		CX,shift
OR		map[SI],CX			;OR operation with the corresponding word(OR will set that bit to '1' and the rest will be unchanged)
JMP		CELL_READ
DEAD_CELL:
MOV		CX,shift
NOT		CX					;negate all the bits of shift(one 0bit at the corresponding location, rest of them will be 0)
AND		map[SI],CX			;use the AND operation(set corresponding bit to 0 because of the one bit which is zero, rest unchanged)
CELL_READ:
SHR		shift,1				;shift to the right, move the position
CMP		shift,0				;if shift equals 0, it means that all bits were read of a given row
JNE		CONTINUE_READ		;if not, continue reading
;read new line from file \r\n-two bytes
MOV		AH,3FH 
MOV		CX,1 
MOV		BX,handle 
INT		21H
MOV		AH,3FH 
MOV		CX,1 
MOV		BX,handle 
INT		21H
ADD		SI,2				;data size is word, increment SI with 2 in order to step to the next element in the array
CMP		SI,31				;check if SI is less than 31
JB		CONTINUE_READ_MAP
JMP		CORRECT				;if everything went ok, jump to CORRECT and print out a message 

ERROR_FILE:
LEA		DX, MSG_10			;error in file, invalid data
MOV		AH,09H	
INT 	21H
JMP		END_READ_FILE

CORRECT:
LEA		DX, MSG_9			;map loaded successfully
MOV		AH,09H	
INT 	21H
END_READ_FILE:
RET
read_map ENDP
;======================================================================================================
;procedure: 	reading a number from keyboard
;limitation: 	2bytes
;DX: 			offset of the initial message
;BX: 			contains the address of memory where the read number will be stored
read_number PROC
MOV		AH,09H				;parameter for printing message
INT 	21H					;print message until end of string(address already in DX)
READ_INIT:
MOV		DX,0
MOV		WORD PTR[BX],0		;start with 0
READ:
MOV		AH,01H
INT		21H					;AL = ASCII code of the pressed key
CMP		AL,0DH				;check if it is enter
JE		END_READ			;ENTER pressed, jump to END_READ
SUB		AL,48
CMP		AL,00H				;check if it is between 0 - 9
JL		INVALID_INPUT
CMP		AL,09H
JG		INVALID_INPUT
MOV		CL,AL				;read digit in CL
MOV		AX,WORD PTR[BX]		;place the current value to AX
PUSH	BX					;save BX on the stack
MOV		BX,10				;10 for multiplying AX with BX
MUL		BX					;multiply, result in DX:AX (if DX>0, an overflow occurs because the proc reads only 2 bytes)
POP		BX					;restore BX(address of the number) from the stack
CMP		DX,0				;if DX<>0 overflow occured(the number occupies more than 2 bytes)
JNE		READ_OVERFLOW		;jump to overflow
MOV		WORD PTR[BX],AX		;move AX(read number so far multiplied with 10)
MOV		CH,0
ADD		WORD PTR[BX], CX	;add the digit to the number(previously it was multiplied by 10)
JMP		READ
INVALID_INPUT:				;message for invalid input
MOV		AH,09H
MOV		DX,OFFSET MSG_3
INT		21H
JMP		READ_INIT
READ_OVERFLOW:				;message for overflow
MOV		AH,09H
MOV		DX,OFFSET MSG_5
INT		21H
JMP		READ_INIT			;error occured(invalid/overflow), read the number again
END_READ:
;the number was read successfully
RET
read_number ENDP
;======================================================================================================
;procedure: 	draw rectangle on the screen
;limitation: 	x<=320, y<=200
;BX:			y coordinate
;CX:			x coordinate
draw_rect PROC
CMP		BX,screen_width		;check if x is less then the width of the screen
JA		DRAW_INVALID		;if it is greater, the drawing is invalid
CMP		CX,screen_height	;check if y is less then the height of the screen`
JA		DRAW_INVALID
MOV		AX,screen_width		;place width into AX
MUL		BX					;multipy with the number of the row in which the top right pixel is situated
ADD		AX,CX				;add the column number of the top right corner
;after this, AX contains the exact location corresponding to x and y parameter
PUSH	AX					;save registers
PUSH	BX
PUSH	CX
MOV		CX,rect_height		
LOOP1:						;outer for loop, rect_height - 0
MOV		DI,AX				;set DI(location on the screen)
PUSH	CX
MOV		CX,rect_width		;inner for loop, rect_width - 0
LOOP2:
MOV		BL,rect_color		
MOV		ES:[DI],BL			;color the location ES:[DI] to the corresponding color
INC		DI					;move to the next column, until width reached
LOOP LOOP2					
POP		CX				
ADD		AX,screen_width		;add the screen width, equivalent to jumping to the next row
LOOP LOOP1				
DRAW_INVALID:
POP		CX					;restore registers from the stack
POP		BX
POP		AX
RET
draw_rect ENDP
;======================================================================================================
;procedure: 	draw grid on the screen
;draws vertical and vertical lines to create a grid
draw_grid PROC
MOV		DX,0					;start with vertical lines, starting at 0
NEXT_COL:
MOV		CX,0					;set y coordinate to 0
MOV		DI,DX					;set DI(used to acces the location in the video memory) - DX contains top row(x,0)
COLCI:
	MOV		BL,1				;blue color
	MOV		ES:[DI],BL			;put a blue pixel
	ADD		DI,screen_width		;jump to the next row, equivalent with y coordinate increment
	INC 	CX					;inc y coordinate
	CMP		CX,160				;check if reached the bottom of the map
JB		COLCI
ADD		DX,rect_width			;DX is always on the first row, so adding rect_width will jump to the next vertical line
CMP		DX,160					;check if the last vertical line was drawn
JB		NEXT_COL				;if not , continue
;draw horizontal lines
MOV		AX,screen_width		
MOV		BX,rect_height
MUL		BX						;AX contains width of rectangle * width of screen(used to jump to the next horizontal line)
MOV		DX,0					;start at 0,0
MOV		CX,0
NEXT_ROWi:
PUSH	CX
MOV		CX,0
MOV		DI,DX					;set DI according to the starting position of the horizontal line
ROWCI:
	MOV		BL,1				;set color to blue
	MOV		ES:[DI],BL			;write into video memory a blue pixel
	INC		DI					;increment DI, ie jump to the next column right next to the previous one
	INC		CX
	CMP		CX,160				;check if end of horizontal line reached
JB		ROWCI
POP		CX
INC		CX
ADD		DX,AX					;jump to the next row (at rect_height * screen_width distance)
CMP		CX,16					;check if all 16 lines were drawn
JB		NEXT_ROWi
RET
draw_grid ENDP
;======================================================================================================
;procedure: drawing the full map on the screen(alive and dead cells)
draw_map PROC
MOV		BX,0
MOV		SI,0
ROW:
MOV		tst,8000H		;tst is used to identify a cell in a row

CMP		SI,0
JE		NEXT
NEXT:
MOV		CX,0
COL_:
MOV		DX,tst			;save tst in DX
SHR		tst,1			;shift tst to get the next position
TEST	map[SI],DX		;check if the cell at position indicated of DX(tst) is 1 or 0
JZ		DRAW_DEAD_CELL	;if it is 0, zero flag is set after the test operation, and a dead cell is drawn
MOV		rect_color,14	;otherwise is a living cell, set color to yellow
JMP		DRAW_NOW
DRAW_DEAD_CELL:
MOV		rect_color,7	;set color to light gray(dead cell)
DRAW_NOW:
CALL	draw_rect		;draw a rectangle(CX - x coordinate(column), BX - y coordinate(row))
ADD		CX,rect_width
CMP		CX,160			;check if all the cells of a row were drawn
JAE		NEXT_ROW		;if yes, jump to the drawing of the next row
JMP		COL_
NEXT_ROW:
ADD		BX,rect_height	;next row, add rect_height to the y coordinate
CMP		BX,160			;check if last row drawn
JAE		DRAW_MAP_END	;if yes, finish
ADD		SI,2			;otherwise move to the next row(increment SI with 2 because the data is WORD - 2bytes)
JMP		ROW
DRAW_MAP_END:
CALL	draw_grid		;after drawing the cells, the grid is drawn to offer a better visual effect
RET

ENDP
;======================================================================================================
;procedure: update the map according to the rules
;no parameters
update PROC
MOV		alive_cells,0		;counts the alive cells, initialize it with zero
MOV		CX,dimension2		
MOV 	SI,0			
NEXT_ROW_UPDATE:
MOV		col,0
MOV		location,8000H		;set location to the first bit	
CMP		SI,0				;check if we examine the first row
JA		NOT_FIRST_ROW
MOV		top,0000H			;if we are in the first row, there is no top row relative to it(we set the top row to zeroes)
MOV		DX,map[SI]			
MOV		middle,DX			;the middle is the current row
MOV		DX,map[SI+2]
MOV		bottom,DX			;the bottom is the row below(map[SI+2])
JMP		UPDATE_NEXT1		;top,bottom,middle set => continue with the verifications
NOT_FIRST_ROW:				;we are not in the first row
MOV		DX,map[SI-2]		;there is a top row relative to the current row(map[SI-2])
MOV		top,DX				
MOV		DX,map[SI]			;the middle is the current row
MOV		middle,DX
CMP		SI,32				;we are surely not in the first row, but we have to check if we are in the last
							;in order to set the bottom correctly
JB		NOT_LAST_ROW		;below 32 => not last row
MOV		bottom,0000H		;otherwise last row, assume the row below has only 0bits
JMP		UPDATE_NEXT1		;continue with the next step(verification)
NOT_LAST_ROW:
MOV		DX,map[SI+2]		;not last row, so we can assign bottom the word at map[SI+2]
MOV		bottom,DX
UPDATE_NEXT1:
PUSH	CX				
MOV		CX,dimension1
NEXT_COLUMN_UPDATE:
CMP		col,0						;compare the column to 0(first column)
JA		NOT_FIRST_COLUMN			;if above 0 then it is not the first column
;first column:
MOV		BX,OFFSET top				;BX contains the offset of the top row relative to the current row
MOV		AH,BYTE PTR[BX+1]			
SHR		AH,1						;we shift the first 8bits to the right in order to obtain a good configuration
MOV		BX,OFFSET middle			;the middle row
MOV		AL,BYTE PTR[BX+1]			;AL contains the first byte of middle(first 8 cells)
SHR		AL,1						;shift AL with 1 to get a good configuration
MOV		BX,OFFSET bottom			;do the same with the bottom line relative to the current line
MOV		BH,BYTE PTR[BX+1]
SHR		BH,1
JMP		NEXT_UPDATE1				;continue
NOT_FIRST_COLUMN:					;we are not in the first column
CMP 	col,1						;check if we are in the second column(no shift needed)
JE		UPDATE_SECOND_COL			
JMP		UPDATE_NEXT2				;if not continue
UPDATE_SECOND_COL:
MOV		BX,OFFSET top				;we must extract the first 8bits of top,bottom and middle to get a good configuration
MOV		AH,BYTE PTR[BX+1]			;of registers AH,AL,BH
MOV		BX,OFFSET middle
MOV		AL,BYTE PTR[BX+1]
MOV		BX,OFFSET bottom
MOV		BH,BYTE PTR[BX+1]
JMP		NEXT_UPDATE1
UPDATE_NEXT2:						;we are neither in the first nor the second column(we can shift left to get the bits needed to the front)
SHL		top,1						;we always shift 1 position 
SHL		middle,1
SHL		bottom,1
MOV		BX,OFFSET top
MOV		AH,BYTE PTR[BX+1]			;and we set registers AH,AL,BH accordingly
MOV		BX,OFFSET middle
MOV		AL,BYTE PTR[BX+1]
MOV		BX,OFFSET bottom
MOV		BH,BYTE PTR[BX+1]
NEXT_UPDATE1:

;count the number of neighbors
CALL	COUNT_NEIGHBOURS			;after this call we have in BL the number of living neighbors of a cell !

MOV		AX,map[SI]
TEST	AX,location					;check if the current cell is alive or not
JZ		DEAD						;if the zero flag is set the result is 0, ie the cell we are looking at is dead
ALIVE:
INC		alive_cells					;otherwise we found a living cell, increase the number of cells alive
CMP		BL,2						;check if the cell has 2 neighbors
JB		DIE							;if it has less than 2, it DIES
CMP		BL,3						;check if the cell has 3 neighbors
JA		DIE							;if it has more than 3, it DIES
JMP		BORN						;otherwise it has 2 or 3 living neighbors, so it wont die
DEAD:								;the cell we are looking at is dead
CMP		BL,3						;check if the dead cell has EXACTLY 3 living neighbors
JNE		DIE							;if not we assume it dies again(no change)
JMP		BORN						;otherwise, if a dead cell has EXACTLY 3 living neighbors, it will born

DIE:
MOV		DX,location					;load the location of the current cell to AX
NOT		DX							;negate to obtain 0 at the specific location, the other bits are 1
AND		newmap[SI],DX				;perform and AND between these two, the current cell will be 0, the others wont change because 
									;the rest of the bits are 1
JMP		NO_ACTION					;after this, jump over the BORN section

BORN:								;the cell has exactly 3 living neighbors`
MOV		DX,location
OR		newmap[SI],DX				;perform OR operation in order to set the corresponding bit to 1(rest wont change because of the 0bits)

NO_ACTION:
SHR		location,1					;shift the location(move to the next cell in the row)
INC		col					
DEC		CX
CMP		CX,0						;check if CX=0(no more columns), if so, we can move on to the next row
JE		UPDATE_NEXT3				;CX=0, jumo to next row
JMP		NEXT_COLUMN_UPDATE			;otehrwise we jump to the next column, but we stay in the same row
UPDATE_NEXT3:
POP		CX							;restore CX from stack(it will contain the number of rows left)
ADD		SI,2						;jump to the next row(data is DW 2 bytes)
DEC		CX							
CMP		CX,0						;check if no more rows are left to be updated
JE		END_UPDATE1					;if no more rows, end the update section
JMP		NEXT_ROW_UPDATE				;otherwise continue to update the next row
END_UPDATE1:
;copy newmap to map(copy the next state into the present state)
PUSH	CX
MOV		CX,dimension2
PUSH	SI
MOV		SI,0
LOOP_COPY:
MOV		DX,newmap[SI]
MOV		map[SI],DX
ADD		SI,2
LOOP LOOP_COPY		;after this finished, the current state is successfully updated
POP		SI			;restore register content
POP		CX
RET
ENDP
;======================================================================================================
;procedure: count the number of living neighbors of a cell
;AH:		start of the top row
;AL:		start of the middle row
;BH:		start of the bottom row
;BL:		stores the number of living cells after the execution of the procedure
COUNT_NEIGHBOURS PROC NEAR
PUSH	CX					;save content of registers on the stack
PUSH	DX
MOV		BL,0				;BL contains the number of living neighbors(assume it is 0)
MOV		CX,3				;3 top neighbors
MOV		DL,80H				;set position
CHECK1:
	MOV		DH,AH		
	AND		DH,DL			;sets zero flag
	JZ		NOT_ALIVE1		;if zero flag set, result of AND operation is zero, results the neighbor cell is dead
	INC		BL				;otherwise the neighbor is alive, increment the number of living neighbors
	NOT_ALIVE1:
	SHR		DL,1			;move position to the neighbor in the right
LOOP CHECK1

MOV		CX,2				;2 middle neighbors
MOV		DL,80H				;set position
CHECK2:
	MOV		DH,AL	
	AND		DH,DL
	JZ		NOT_ALIVE2		;same principle as before
	INC		BL
	NOT_ALIVE2:
	SHR		DL,2			;shift two places(jump over the cell itself)
LOOP CHECK2

MOV		CX,3				;3 bottom neighbors
MOV		DL,80H
CHECK3:
	MOV		DH,BH
	AND		DH,DL
	JZ		NOT_ALIVE3		;same principle as before
	INC		BL
	NOT_ALIVE3:
	SHR		DL,1
LOOP CHECK3
POP		DX					;restore the content of the registers from the stack
POP		CX
RET
COUNT_NEIGHBOURS ENDP
;======================================================================================================
CODE ENDS
END MAIN