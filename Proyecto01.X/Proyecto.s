;******************************************************************************
; Proyecto_01
;*****************************************************************************
; Archivo:	Proyecto.s
; Dispositivo:	PIC16F887
; Autor:	Marco Duarte
; Compilador:	pic-as (v2.30), MPLABX V5.45
;******************************************************************************

PROCESSOR 16F887
#include <xc.inc>

;******************************************************************************
; Palabras de configuracion 
;******************************************************************************

; CONFIG1
  CONFIG  FOSC =    INTRC_NOCLKOUT   ; Oscillator Selection bits (XT oscillator: Crystal/resonator on RA6/OSC2/CLKOUT and RA7/OSC1/CLKIN)
  CONFIG  WDTE =    OFF            ; Watchdog Timer Enable bit (WDT disabled and can be enabled by SWDTEN bit of the WDTCON register)
  CONFIG  PWRTE =   OFF            ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  MCLRE =   OFF           ; RE3/MCLR pin function select bit (RE3/MCLR pin function is digital input, MCLR internally tied to VDD)
  CONFIG  CP =	    OFF              ; Code Protection bit (Program memory code protection is disabled)
  CONFIG  CPD =	    OFF             ; Data Code Protection bit (Data memory code protection is disabled)
  CONFIG  BOREN =   OFF           ; Brown Out Reset Selection bits (BOR disabled)
  CONFIG  IESO =    OFF            ; Internal External Switchover bit (Internal/External Switchover mode is disabled)
  CONFIG  FCMEN =   OFF           ; Fail-Safe Clock Monitor Enabled bit (Fail-Safe Clock Monitor is disabled)
  CONFIG  LVP =	    ON              ; Low Voltage Programming Enable bit (RB3/PGM pin has PGM function, low voltage programming enabled)

; CONFIG2
  CONFIG  BOR4V =   BOR40V        ; Brown-out Reset Selection bit (Brown-out Reset set to 4.0V)
  CONFIG  WRT =	    OFF             ; Flash Program Memory Self Write Enable bits (Write protection off)


;******************************************************************************
; Variables
;******************************************************************************
      ; Se definen variables 
PSECT udata_shr ;Common memory
    
    W_TEMP:	    ; Variable para que se guarde w
	DS 1
    STATUS_TEMP:    ; Variable para que guarde status
	DS 1
    disp_var:
	DS  8
    
PSECT udata_bank0 
    residuo:
	DS  1
    stage:
	DS  1
    selector:
	DS  1
    flags:
	DS  1
    sem:
	DS 1
    control01:
	DS 1
    control02:
	DS 1
    count01:
	DS 1
    timer1:
	DS 1
    timer2:
	DS 1
    timer3:
	DS 1
    control03:
	DS 1
    control04:
        DS 1
    flagsem:
	DS 1
    control05:
	DS 1
    control06:
        DS 1
    control07:
	DS 1
    control08:
        DS 1
    flagst:
        DS 1
    countsel:
        DS 1
    preptim01:
        DS 1
    preptim02:
        DS 1
    preptim03:
        DS 1
    tiempo01:
        DS 1
    tiempo02:
        DS 1
    tiempo03:
        DS 1
    dispsele:
	DS 1

GLOBAL sem
GLOBAL count01
GLOBAL timer1
GLOBAL timer2
GLOBAL timer3
;******************************************************************************
; Vector Reset
;******************************************************************************
PSECT resVect, class=code, abs, delta=2
;--------------------------vector reset-----------------------------------------
ORG 00h        ;posicion 0000h para el reset
resetVec:
    PAGESEL main
    goto main
;******************************************************************************
; Interrupciones
;******************************************************************************
PSECT code, delta=2, abs
ORG 04h 
    
push:			; Mover las variables temporales a w
    movwf   W_TEMP
    swapf   STATUS, W
    movwf   STATUS_TEMP

isr:	
    BANKSEL PORTB
    btfsc   RBIF	; Revisar si hay interrupciones en el puerto b
    call    active
	
    btfsc   T0IF	; Revisar si hay overflow del timer0
    call    int_tmr
    
    btfsc   TMR1IF	; Revisar si hay overflow del timer1
    call    int_tmr1
 
pop:			; Regresar w al status
    swapf   STATUS_TEMP, W
    movwf   STATUS
    swapf   W_TEMP, F
    swapf   W_TEMP, W
    retfie
    
 ;------------------------Sub rutinas de interrupcion--------------------------
 int_tmr1:	; Interruocion timer1
    BANKSEL TMR1H   
    movlw   0xE1       ; Modifico los registros del timer1
    movwf   TMR1H
    
    BANKSEL TMR1L
    movlw   0x7C
    movwf   TMR1L
    
    incf    count01	; Se incrementa la variable para el timer
    bcf	    TMR1IF
return  
    
int_tmr:
    call    reset0	; Se limpia el TMR0
    clrf    PORTD
    
; Lo que se busca hacer aca es revisar que display esta activado he ir al sig.
    btfsc   flags, 0	; Flags es una variable 
    goto    disp_02
    
    btfsc   flags, 1
    goto    disp_03
    
    btfsc   flags, 2
    goto    disp_04
    
    btfsc   flags, 3
    goto    disp_05
    
    btfsc   flags, 4
    goto    disp_06
    
    btfsc   flags, 5
    goto    disp_07
    
    btfsc   flags, 6
    goto    disp_08
     
    ; Se crean varias rutinas internas para activar los displays
disp_01:
    movf    control03, w
    movwf   PORTC
    bsf	    PORTD, 0
    goto    next_disp
disp_02:
    movf    control04, W
    movwf   PORTC
    bsf	    PORTD, 1
    goto    next_disp01
disp_03:
    movf    control05, W
    movwf   PORTC
    bsf	    PORTD, 2
    goto    next_disp02
disp_04:
    movf    control06, W
    movwf   PORTC
    bsf	    PORTD, 3
    goto    next_disp03
disp_05:
    movf    control07, W
    movwf   PORTC
    bsf	    PORTD, 4
    goto    next_disp04
disp_06:
    movf    control08, W
    movwf   PORTC
    bsf	    PORTD, 5
    goto    next_disp05 
disp_07:
    movf    control01, W
    movwf   PORTC
    bsf	    PORTD, 6
    goto    next_disp06
disp_08:
    movf    control02, W
    movwf   PORTC
    bsf	    PORTD, 7
    goto    next_disp07
    
next_disp:  ; Se crean XOR para cada display en modo de hacer rotaciones
    MOVLW   00000001B   ; Se empieza con un bit
    XORWF   flags, 1
    RETURN
next_disp01:
    MOVLW   00000011B
    xorwf   flags, 1
    return
next_disp02:
    movlw   00000110B
    xorwf   flags, 1
    return
next_disp03:
    movlw   00001100B
    xorwf   flags, 1
    return
next_disp04:
    movlw   00011000B
    xorwf   flags, 1
    return
next_disp05:
    movlw   00110000B
    xorwf   flags, 1
    return
next_disp06:
    movlw   01100000B
    xorwf   flags, 1
    return
next_disp07:
    clrf    flags
return
    
active:   ; La subrutina para incrementar y decrementar
    btfss   PORTB, 0	; Se revisa si se apacha el boton 1
    call    up
    btfss   PORTB, 1	; Se revisa si se apacha el boton 2
    call    down	; Se decrementa1
    btfss   PORTB, 2
    call    selstage
    bcf	    RBIF
return 

 
;******************************************************************************
; Configuracion de tabla
;******************************************************************************
PSECT code, delta=2, abs
ORG 100h    ;posicion para el codigo
 
; Tabla de la traduccion de binario a decimal
table:
    clrf	PCLATH
    bsf		PCLATH, 0
    andlw	0x0F	    ; Se pone como limite F , en hex 15
    addwf	PCL
    RETLW	00111111B   ;0
    RETLW	00000110B   ;1
    RETLW	01011011B   ;2
    RETLW	01001111B   ;3
    RETLW	01100110B   ;4
    RETLW	01101101B   ;5
    RETLW	01111101B   ;6
    RETLW	00000111B   ;7
    RETLW	01111111B   ;8
    RETLW	01101111B   ;9
    RETLW	01110111B   ;A
    RETLW	01111100B   ;B
    RETLW	00111001B   ;C
    RETLW	01011110B   ;D
    RETLW	01111001B   ;E
    RETLW	01110001B   ;F
    
    
;******************************************************************************
; Configuracion 
;******************************************************************************
    ; Esta es la configuracion de los pines
ORG 118h
main:
    ; Configurar puertos digitales
    BANKSEL ANSEL	; Se selecciona bank 3
    clrf    ANSEL	; Definir puertos digitales
    clrf    ANSELH
    
    ; Configurar puertos de salida A
    BANKSEL TRISA	; Se selecciona bank 1
    bcf	    TRISA,  0	; R0 lo defino como output
    bcf	    TRISA,  1	; R1 lo defino como output
    bcf	    TRISA,  2	; R2 lo defino como output
    bcf	    TRISA,  3	; R3 lo defino como output
    bcf	    TRISA,  4	; R4 lo defino como output
    bcf	    TRISA,  5	; R5 lo defino como output
    bcf	    TRISA,  6	; R5 lo defino como output
    bcf	    TRISA,  7	; R5 lo defino como output

    ; Configurar puertos de salida B
    BANKSEL TRISB	; Se selecciona bank 1
    bsf	    TRISB,  0	; R0 lo defino como input
    bsf	    TRISB,  1	; R1 lo defino como input
    bsf	    TRISB,  2	; R2 lo defino como input
    bsf	    TRISB,  4	; R2 lo defino como input
    bcf	    TRISB,  5	; R5 lo defino como onput
    bcf	    TRISB,  6	; R6 lo defino como onput
    bcf	    TRISB,  7	; R7 lo defino como onput
        
    ; Configurar puertos de salida C
    BANKSEL TRISC	; Se selecciona bank 1
    bcf	    TRISC,  0	; R0 lo defino como output
    bcf	    TRISC,  1	; R1 lo defino como output
    bcf	    TRISC,  2	; R2 lo defino como output
    bcf	    TRISC,  3	; R3 lo defino como output
    bcf	    TRISC,  4	; R4 lo defino como output
    bcf	    TRISC,  5	; R5 lo defino como output
    bcf	    TRISC,  6	; R6 lo defino como output
    bcf	    TRISC,  7	; R7 lo defino como output
    
    ; Configurar puertos de salida D
    BANKSEL TRISD	; Se selecciona el bank 1
    bcf	    TRISD,  0	; R0 lo defino como output
    bcf	    TRISD,  1	; R1 lo defino como output
    bcf	    TRISD,  2	; R2 lo defino como output
    bcf	    TRISD,  3	; R3 lo defino como output
    bcf	    TRISD,  4	; R4 lo defino como output
    bcf	    TRISD,  5	; R5 lo defino como output
    bcf	    TRISD,  6	; R6 lo defino como output
    bcf	    TRISD,  7	; R7 lo defino como output
    
    ;***************Configuracion de Pull-up interno***************************    
    ; Poner puerto b en pull-up
    BANKSEL OPTION_REG
    bcf	    OPTION_REG, 7
    
    BANKSEL WPUB
    bsf	    WPUB, 0	; Se activa el pull-up interno
    bsf	    WPUB, 1	; Se activa el pull-up interno
    bsf	    WPUB, 2	; Los demas pull-up se desactivan
    bcf	    WPUB, 3
    bcf	    WPUB, 4
    bcf	    WPUB, 5
    bcf	    WPUB, 6
    bcf	    WPUB, 7
    ;************************************************************************** 
    
    ; Se llama las configuraciones del clock
    call    clock		; Llamo a la configurcion del oscilador interno
    
    ;***************Configuracion de interrupciones****************************
    BANKSEl IOCB	; Activar interrupciones
    movlw   00000111B	; Activar las interrupciones en RB0 y RB1
    movwf   IOCB
    
    BANKSEL INTCON
    bcf	    RBIF
    
        ; Bits de interrupcion
    bsf	    GIE		; Interrupcion global
    bsf	    RBIE	; Interrupcion puerto b
    bsf	    T0IE	; Interrupcion timer0
    bcf	    T0IF
    ;**************************************************************************
    
    ;***************Configuracion de Timer0************************************
    BANKSEL OPTION_REG
    BCF	    T0CS
    BCF	    PSA		;prescaler asignado al timer0
    BSF	    PS0		;prescaler tenga un valor 1:256
    BSF	    PS1
    BSF	    PS2
    ;**************************************************************************
    
    ;****************Configuracion de Timer1***********************************
    BANKSEL T1CON
    bsf	    T1CKPS1	;prescaler 1:8
    bsf	    T1CKPS0
    bcf	    TMR1CS	;internal clock
    bsf	    TMR1ON	;habilitar timer1
    ;**************************************************************************

    ;****************Configuracion de Timer2***********************************
    BANKSEL T2CON
    movlw   1001110B     ;1001 para el postcaler, 1 timer 2 on, 10 precaler 16
    movwf   T2CON
    ;**************************************************************************
    
    ;****************Confiuracion default**************************************
    ; Se define la variable inicial del contador de seleccion
    movlw   10
    movwf   sem
    movlw   5
    movwf   tiempo01
    movlw   5
    movwf   tiempo02
    movlw   5
    movwf   tiempo03
    
    ; Se inicializan todos los semaforos en rojo
;    bsf	    PORTA, 0
;    bsf	    PORTA, 3
;    bsf	    PORTA, 6
    ;**************************************************************************
    
    ; Limpiar los puertos
    BANKSEL PORTA
    clrf    PORTA
    clrf    PORTB
    clrf    PORTC
    clrf    PORTD
    clrf    PORTE
    
;******************************************************************************
; Loop Principal
;******************************************************************************
    loop:
    
    BANKSEL PORTA
    bsf	    PORTA, 0
    bsf	    PORTA, 3
    bsf	    PORTA, 6
    
    btfsc   dispsele, 0
    call    division 
    btfsc   dispsele, 1
    call    aceptar
    
    call    timers
    call    division01
    call    division02
    call    division03
    
    goto    loop
;******************************************************************************
; Sub-Rutinas 
;******************************************************************************
    
reset0:
    ;BANKSEL PORTA
    movlw   255	    ; Tiempo de intruccion
    movwf   TMR0
    bcf	    T0IF    ; Volver 0 al bit del overflow
    return
 
clock:		    ; Se configura el oscilador interno
    BANKSEL OSCCON
    bcf	    IRCF2   ; Se selecciona 010
    bsf	    IRCF1   
    bcf	    IRCF0   ; Frecuencia de 250 KHz
    bsf	    SCS	    ; Activar oscilador interno
    return
    
division:   ; Se crea la subrutina de la separacion de valores
    clrf	selector
    clrf	residuo
    bcf		STATUS, 0
    movf	sem, 0	    ; Se mueve lo que hay en el contador a w
    movwf	selector	    ; Se mueve w a la variable residuos		    ; Empieza la parte de las decenas
    movlw	10		    ; Se mueve 10 a w
    incf	residuo
    subwf	selector, f	    ; Se le resta a residuos 10
    btfsc	STATUS, 0	    ; Se verifica la bandera
    goto	$-3
    decf	residuo		    ; Se incrementa la variable decenas
    addwf	selector
    movf	residuo, w
    call	table
    movwf	control01
    movf	selector, w
    call	table
    movwf	control02    
    return
    
division01:   ; Se crea la subrutina de la separacion de valores
    clrf	selector
    clrf	residuo
    bcf		STATUS, 0
    movf	timer1, w	    ; Se mueve lo que hay en el contador a w
    movwf	selector	    ; Se mueve w a la variable residuos		    ; Empieza la parte de las decenas
    movlw	10		    ; Se mueve 10 a w
    incf	residuo
    subwf	selector, f	    ; Se le resta a residuos 10
    btfsc	STATUS, 0	    ; Se verifica la bandera
    goto	$-3
    decf	residuo		    ; Se incrementa la variable decenas
    addwf	selector
    movf	residuo, w
    call	table
    movwf	control03
    movf	selector, w
    call	table
    movwf	control04    
    return
    
division02:   ; Se crea la subrutina de la separacion de valores
    clrf	selector
    clrf	residuo
    bcf		STATUS, 0
    movf	timer2, w	    ; Se mueve lo que hay en el contador a w
    movwf	selector	    ; Se mueve w a la variable residuos		    ; Empieza la parte de las decenas
    movlw	10		    ; Se mueve 10 a w
    incf	residuo
    subwf	selector, f	    ; Se le resta a residuos 10
    btfsc	STATUS, 0	    ; Se verifica la bandera
    goto	$-3
    decf	residuo		    ; Se incrementa la variable decenas
    addwf	selector
    movf	residuo, w
    call	table
    movwf	control05
    movf	selector, w
    call	table
    movwf	control06    
    return
    
division03:   ; Se crea la subrutina de la separacion de valores
    clrf	selector
    clrf	residuo
    bcf		STATUS, 0
    movf	timer3, w	    ; Se mueve lo que hay en el contador a w
    movwf	selector	    ; Se mueve w a la variable residuos		    ; Empieza la parte de las decenas
    movlw	10		    ; Se mueve 10 a w
    incf	residuo
    subwf	selector, f	    ; Se le resta a residuos 10
    btfsc	STATUS, 0	    ; Se verifica la bandera
    goto	$-3
    decf	residuo		    ; Se incrementa la variable decenas
    addwf	selector
    movf	residuo, w
    call	table
    movwf	control07
    movf	selector, w
    call	table
    movwf	control08    
    return
    
aceptar:
    movlw	10
    call	table
    movwf	control01
    movlw	12
    call	table
    movwf	control02
    return
    
selstage:
    ;BANKSEL PORTA
    incf    stage
    
    btfsc   flagst, 0	; flagst es una variable 
    goto    option01
    
    btfsc   flagst, 1
    goto    option02
    
    btfsc   flagst, 2
    goto    option03
    
    btfsc   flagst, 3
    goto    back
    
    
       
option0:
;    call    division    
    bcf	    STATUS, 2
    movlw   1
    movwf   countsel
    movf    stage, w
    subwf   countsel, w
    btfss   STATUS, 2
    goto    $+3
    bsf	    PORTB, 5
    bsf	    flagst, 0
    bsf	    dispsele, 0
    return
option01:
;    call    division
;    movf    sem, w
;    movwf   preptim01
    bcf	    STATUS, 2
    movlw   2
    movwf   countsel
    movf    stage, w
    subwf   countsel, w
    btfss   STATUS, 2
    goto    $+5
    bcf	    PORTB, 5
    bsf	    PORTB, 6
    bcf	    flagst, 0
    bsf	    flagst, 1
    return
option02:
;    call    division
;    movf    sem, w
;    movwf   preptim02
    bcf	    STATUS, 2
    movlw   3
    movwf   countsel
    movf    stage, w
    subwf   countsel, w
    btfss   STATUS, 2
    goto    $+5
    bsf	    PORTB, 5
    bsf	    PORTB, 6
    bcf	    flagst, 1
    bsf	    flagst, 2
    return
option03:
    
;    movf    sem, w
;    movwf   preptim03
    bcf	    STATUS, 2
    movlw   4
    movwf   countsel
    movf    stage, w
    subwf   countsel, w
    btfss   STATUS, 2
    goto    $+6
    bcf	    PORTB, 5
    bcf	    PORTB, 6
    bsf	    PORTB, 7
    bcf	    flagst, 2
    bsf	    flagst, 3
    bcf	    dispsele, 0
    bsf	    dispsele, 1
;    btfsc   PORTB, 0
;    call    configuracion
    return
back:
    clrf    control01
    clrf    control02
    bcf	    PORTB, 7
    clrf    flagst
    clrf    stage
    clrf    dispsele
    return
    
up:
    incf    sem
    bcf     STATUS, 2
    movlw   21
    subwf   sem, w
    btfss   STATUS, 2
    goto    $+3
    movlw   10
    movwf   sem 
    return
    
down:
    decf    sem
    bcf	    STATUS, 2
    movlw   9
    subwf   sem, w
    btfss   STATUS, 2
    goto    $+3
    movlw   20
    movwf   sem
    return      
    
timers:
    
    btfsc   flagsem, 0	; Flags es una variable 
    goto    sem02
    
    btfsc   flagsem, 1
    goto    sem03
    
    btfsc   flagsem, 2
    goto    clear
    
  sem01:
    bcf	    STATUS, 2
    movf    tiempo01, w
    movwf   timer1
    movf    count01, w
    subwf   timer1, 1
    btfss   STATUS, 2
    goto    $+2
    bsf	    flagsem, 0
    return
  sem02: 
    bcf	    STATUS, 2
    movf    tiempo02, w
    addwf   tiempo01, w
    movwf   timer2
    movf    count01, w
    subwf   timer2, 1
    btfss   STATUS, 2
    goto    $+3
    bcf	    flagsem, 0
    bsf	    flagsem, 1
    return
  sem03:
    bcf	    STATUS, 2
    movf    tiempo03, w
    addwf   tiempo01, w
    addwf   tiempo02, w
    movwf   timer3
    movf    count01, w
    subwf   timer3, 1
    btfss   STATUS, 2
    goto    $+2
    bcf	    flagsem, 1
    bsf	    flagsem, 2
    return
  clear:
    clrf    count01
    clrf    flagsem
    bcf	    TMR1IF
    return
    
activate:
    bsf	    PORTB, 5
    bsf	    PORTB, 6
    bsf	    PORTB, 7
    return
    
semaforos:
   btfsc   flagsem, 0	; Flags es una variable 
    goto    sema02
    
    btfsc   flagsem, 1
    goto    sema03
    
    btfsc   flagsem, 2
    goto    reseteo 
    
  sema01:

    return
  sema02:   

    return
  sema03:
 
    return 
  reseteo:
    
    return
    
configuracion:
    movf    preptim01, w
    movlw   tiempo01
    movf    preptim02, w
    movlw   tiempo02
    movf    preptim03, w
    movlw   tiempo03
    return
    
    
    END
