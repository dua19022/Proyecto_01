;******************************************************************************
; Laboratorio 05
;*****************************************************************************
; Archivo:	Lab_05.s
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
      ; Se definen variables , pero por el momento no las estoy usando
PSECT udata_shr ;Common memory
    
    W_TEMP:	    ; Variable para que se guarde w
	DS 1
    STATUS_TEMP:    ; Variable para que guarde status
	DS 1
    nibble:
	DS  2
    count:
	DS 1
    disp_var:
	DS  5
    flags:
	DS  1

PSECT udata_bank0 
    var:
	DS  1
    Sem01:
	DS  1
    dis01:
	DS  1
    residuos:
	DS  1
    centenas:
	DS  1
    decenas:
	DS  1
    control_selec:
	DS  1
    control_sum:
	DS  1
    stage:
	DS  1
    flag_stage:
	DS  1
    selector:
	DS  1
	
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
    ;btfsc   RBIF	; Revisar si hay interrupciones en el puerto b
    call    int_ocb	; Se llama a la subrutina de los botones	
    ;btfsc   T0IF	; Revisar si hay overflow del timer0
    call    int_tmr
 
pop:			; Regresar w al status
    swapf   STATUS_TEMP, W
    movwf   STATUS
    swapf   W_TEMP, F
    swapf   W_TEMP, W
    retfie
    
 ;------------------------Sub rutinas de interrupcion--------------------------
 
int_tmr:
    call    reset0	; Se limpia el TMR0
    bcf	    PORTD, 0	; Se limpian todos los puertos que van a los transistores
    bcf	    PORTD, 1
    bcf	    PORTD, 2
    bcf	    PORTD, 3
    bcf	    PORTD, 4
    bcf	    PORTD, 5
    bcf	    PORTD, 6
    bcf	    PORTD, 7
    
; Lo que se busca hacer aca es revisar que display esta activado he ir al sig.
    btfsc   flags, 0	; Flags es una variable 
    goto    disp_02
    
    btfsc   flags, 1
    goto    disp_03
    
    btfsc   flags, 2
    goto    disp_04
    
    btfsc   flags, 3
    goto    disp_05
 
    ; Se crean varias rutinas internas para activar los displays
disp_01:
    movf    disp_var, w
    movwf   PORTC
    bsf	    PORTD, 7
    goto    next_disp
disp_02:
    movf    disp_var+1, W
    movwf   PORTC
    bsf	    PORTD, 6
    goto    next_disp01
disp_03:
    movf    disp_var+2, W
    movwf   PORTC
    bsf	    PORTD, 5
    goto    next_disp02
disp_04:
    movf    disp_var+3, W
    movwf   PORTC
    bsf	    PORTD, 6
    goto    next_disp03
disp_05:
    movf    disp_var+4, W
    movwf   PORTC
    bsf	    PORTD, 7
    goto    next_disp04

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
    clrf    flags
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

    ; Configurar puertos de salida B
    BANKSEL TRISB	; Se selecciona bank 1
    bsf	    TRISB,  0	; R0 lo defino como input
    bsf	    TRISB,  1	; R1 lo defino como input
    bsf	    TRISB,  2	; R1 lo defino como input
    bcf	    TRISB,  5	; R0 lo defino como onput
    bcf	    TRISB,  6	; R0 lo defino como onput
    bcf	    TRISB,  7	; R1 lo defino como onput
        
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
    
     ; Configurar puertos de salida E
    BANKSEL TRISE	; Se selecciona el bank 1
    bcf	    TRISE,  0	; R0 lo defino como output
    bcf	    TRISE,  1	; R1 lo defino como output
    bcf	    TRISE,  2	; R1 lo defino como output
    
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
    
    ; Se llama las configuraciones del clock
    call    clock		; Llamo a la configurcion del oscilador interno
    ;call    reset0
    
    ; Interrupciones
    BANKSEl IOCB	; Activar interrupciones
    movlw   00000011B	; Activar las interrupciones en RB0 y RB1
    movwf   IOCB
    
    BANKSEL INTCON
    bcf	    RBIF
    
      ; Configuracion de Timer0
    BANKSEL OPTION_REG
    BCF	    T0CS
    BCF	    PSA		;prescaler asignado al timer0
    BSF	    PS0		;prescaler tenga un valor 1:256
    BSF	    PS1
    BSF	    PS2
    
    ; Bits de interrupcion
    bsf	    GIE		; Interrupcion global
    bsf	    RBIE	; Interrupcion puerto b
    bsf	    T0IE	; Interrupcion timer0
    bcf	    T0IF
    
           
    
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
    
    btfsc   flag_stage, 0
    call    selstage
    btfsc   flag_stage, 1
    call    up
    btfsc   flag_stage, 2
    call    down
    movfw   stage, w
    movlw   
    ;BANKSEL PORTA
    ;call    prep_nib	; Se mandan los nibbles a cada display
   ; BANKSEL PORTA
   ; call    division	; Se ejecuta la subrutina de la operacion
    
    goto    loop
;******************************************************************************
; Sub-Rutinas 
;******************************************************************************
int_ocb:
    banksel PORTB
    btfss   PORTB, 2
    bsf	    flag_stage, 0
    btfss   PORTB, 0
    bsf	    flag_stage, 1
    btfss   PORTB, 1
    bsf	    flag_stage, 2
    bcf	RBIF
    return
    
prep_nib:   ; Se mandan los valores que se quieren desplegar en el 7 segmentos
    movf    selector, w
    call    table
    movwf   disp_var	; Display 1
    
    movf    decenas, w
    call    table
    movwf   disp_var+1	; Display 2
    /*
    movf    centenas, W
    call    table
    movwf   disp_var+2	; Display 3
    
    movf    decenas, W
    call    table
    movwf   disp_var+3	; Display 4
    
    movf    residuos, W
    call    table
    movwf   disp_var+4	; Display 5
    */
    return

reset0:
    ;BANKSEL PORTA
    movlw   253	    ; Tiempo de intruccion
    movwf   TMR0
    bcf	    T0IF    ; Volver 0 al bit del overflow
    return
    
active:   ; La subrutina para incrementar y decrementar
    btfss   PORTB, 0	; Se revisa si se apacha el boton 1
    incf    PORTA	; Se incrmenta
    btfss   PORTB, 1	; Se revisa si se apacha el boton 2
    decf    PORTA	; Se decrementa
    bcf	    RBIF
    return 
    
clock:		    ; Se configura el oscilador interno
    BANKSEL OSCCON
    bcf	    IRCF2   ; Se selecciona 010
    bsf	    IRCF1   
    bcf	    IRCF0   ; Frecuencia de 250 KHz
    bsf	    SCS	    ; Activar oscilador interno
    return
    
division:   ; Se crea la subrutina de la separacion de valores
    movf	PORTA, 0	    ; Se mueve lo que hay en el contador a w
    movwf	selector	    ; Se mueve w a la variable residuos
    
    clrf	decenas		    ; Empieza la parte de las decenas
    movlw	10		    ; Se mueve 10 a w
    subwf	selector, 0	    ; Se le resta a residuos 10
    btfsc	STATUS, 0	    ; Se verifica la bandera
    incf	decenas		    ; Se incrementa la variable decenas
    btfsc	STATUS, 0	    ; Se verifica la bandera
    movwf	selector	    ; Se usa residuos como unidades ya que es lo que sobra
    btfsc	STATUS, 0	    ; Se verifica la bandera
    goto	$-7		    ;
    btfss	STATUS, 0	    ;    
    
    return
    
selstage:
    BANKSEL PORTA
    incf    stage
    bcf	    STATUS, 2
    movlw   5
    subwf   stage, w
    btfss   STATUS, 2
    goto    $+3
    movlw    0
    movwf   stage
    bcf	    flag_stage, 0 
    return
    
up:
    incf    selector
    bcf	    STATUS, 2
    movlw   21
    subwf   selector, w
    btfss   STATUS, 2
    goto    $+3
    movlw   10
    movwf   selector
    bcf	    flag_stage, 1    
    return
    
down:
    decf    selector
    bcf	    STATUS, 2
    movlw   9
    subwf   selector, w
    btfss   STATUS, 2
    goto    $+3
    movlw   20
    movwf   selector
    bcf	    flag_stage, 1    
    return    
    
option01:
    btfss   T0IF	; Sumar cuando llegue al overflow el timer0
    goto    $-1
    call    reset0	; Regresa el overflow a 0
    ;incf    cont01
    ;movf    cont01, w
    ;subwf   charge, w
    ;movf    Sem01, w
    ;call    table
   ; movwf   dis01
   return
 
option02:
    
    return
    
option03:
    
    return
    
    END
