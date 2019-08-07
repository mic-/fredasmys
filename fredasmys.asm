; fredASMys - An x86 assembly YM player for Windows
; Mic, 2019

.686
.model flat,stdcall
option casemap:none

include \masm32\include\windows.inc
include \masm32\include\kernel32.inc
include \masm32\include\user32.inc
include .\bass.inc

includelib \masm32\lib\kernel32.lib
includelib \masm32\lib\user32.lib
includelib .\bass.lib

wsprintf equ <wsprintfA>

.data
    SAMPLE_RATE     equ 44100
    YM_FRAME_SAMPLES equ (SAMPLE_RATE/50)
    MAX_DIGIDRUMS   equ 256
    TIMER_MULT      equ 4

    ; YM registers
    YM_TONEGENL_A	equ 0
    YM_TONEGENH_A	equ 1
    YM_TONEGENL_B	equ 2
    YM_TONEGENH_B	equ 3
    YM_TONEGENL_C	equ 4
    YM_TONEGENH_C	equ 5
    YM_NOISE_CTRL	equ 6
    YM_MIXER        equ 7
    YM_LEVEL_A      equ 8
    YM_LEVEL_B      equ 9
    YM_LEVEL_C      equ 10
    YM_ENVE_FREQL   equ 11
    YM_ENVE_FREQH   equ 12
    YM_ENVE_SHAPE   equ 13

    ; Software effect types
    SFX_SID_VOICE   equ 0
    SFX_DIGI_DRUM   equ 1
    SFX_SINUS_SID   equ 2
    SFX_SYNC_BUZZ   equ 3
    SFX_NONE        equ 64

    ; Digidrum formats
    DIGI_DRUM_U4	equ 0
    DIGI_DRUM_U8	equ 1
    DIGI_DRUM_S8	equ 2

    ; BASS states
    STATE_UNDEFINED equ 0
    STATE_INITED	equ 1
    STATE_STARTED	equ 2

    ; 16-step envelope
    YM_VOL_TB     dw 42,59,84,118,167,237,335,473,668,944,1333,1883,2661,3758,5309,7500
    ; 32-step envelope
    YM2149_VOL_TB dw 35,42,50,59,70,84,99,118,141,167,199,236,281,334,397,472,562,668,793,943,1121,1333,1584,1883,2238,2660,3162,3758,4466,5309,6310,7500

    YM_DIGIDRUM_8TO4_TB db 0,0,1,1,3,4,4,5,5,5,6,6,6,6,7,7
        db 7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9
        db 9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10
        db 10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11
        db 11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12
        db 12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
        db 12,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13
        db 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
        db 13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
        db 13,13,13,13,13,13,13,13,13,13,14,14,14,14,14,14
        db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
        db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
        db 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14
        db 14,14,14,14,14,14,14,14,14,14,15,15,15,15,15,15
        db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
        db 15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15
    
    SINUS_SID_TB dw 0000h,0000h,0000h,0000h,0000h,0000h,0000h,0000h
                dw 00FAh,01AAh,01F4h,01AAh,00F9h,0049h,0000h,0049h
                dw 01F4h,0355h,03E8h,0355h,01F3h,0092h,0000h,0092h
                dw 02EEh,0500h,05DCh,0500h,02EDh,00DBh,0000h,00DBh
                dw 03E8h,06ABh,07D0h,06ABh,03E7h,0124h,0000h,0124h
                dw 04E2h,0855h,09C4h,0855h,04E1h,016Eh,0000h,016Eh
                dw 05DCh,0A00h,0BB8h,0A00h,05DBh,01B7h,0000h,01B7h
                dw 06D6h,0BABh,0DACh,0BABh,06D5h,0200h,0000h,0200h
                dw 07D0h,0D56h,0FA0h,0D56h,07CFh,0249h,0000h,0249h
                dw 08CAh,0F00h,1194h,0F00h,08C9h,0293h,0000h,0293h
                dw 09C4h,10ABh,1388h,10ABh,09C3h,02DCh,0000h,02DCh
                dw 0ABEh,1256h,157Ch,1256h,0ABDh,0325h,0000h,0325h
                dw 0BB8h,1401h,1770h,1401h,0BB7h,036Eh,0000h,036Eh
                dw 0CB2h,15ACh,1964h,15ACh,0CB1h,03B7h,0000h,03B7h
                dw 0DACh,1756h,1B58h,1756h,0DABh,0401h,0000h,0401h
                dw 0EA6h,1901h,1D4Ch,1901h,0EA5h,044Ah,0000h,044Ah

    TIMER_PRESCALER_TB dd 0,4*TIMER_MULT,10*TIMER_MULT,16*TIMER_MULT,50*TIMER_MULT,64*TIMER_MULT,100*TIMER_MULT,200*TIMER_MULT

    szNameFile db "callme.ym",0
    szInfoFormat db "fredASMys - Mic, 2019",13,10,"Playing %s - %s",13,10,13,10,"Press any key to quit..",0

    lpFileData      dd 0
    bassState       dd STATE_UNDEFINED

    masterVolume REAL4 0.30f

    prevFiltered dd 0
    prevUnfiltered dd 0

.data?
    align 4
    szBuffer		db 4096 DUP(?)
    kbdState	    db 256 DUP(?)

    ymRegs			db 16 DUP(?)

    digiDrumPtr		dd 32 DUP(?)
    digiDrumLen		dd 32 DUP(?)
    digiDrumFormat	dd ?

    lpYmData        dd ?
    lpYmRegStream   dd ?
    lpYmEnvTable	dd ?
    
    lpYmSongTitle   dd ?
    lpYmSongAuthor  dd ?
    lpYmSongComment dd ?
    
    speedToneNoise  dd ?
    speedEnve       dd ?
    speedTimer1     dd ?
    
    frame           dd ?
    sampleCount     dd ?
    numFrames		dd ?
    loopFrame		dd ?
    loopOffs		dd ?

    hConsole		dd ?
    hConsoleInput	dd ?

    hFile			dd ?
    ymFileSize		dd ?
    bytesRead		dd ?

    hStream			dd ?
    hQuitEvent		dd ?

    YM_CHANNEL STRUCT
        counter DWORD ?
        period DWORD ?
        num DWORD ?         ; 0=A, 1=B, 2=C
        lpCurVol DWORD ?
        volume WORD ?
        mode db ?
        tone db ?
        noise db ?
        phase db ?
    YM_CHANNEL ENDS

    YM_ENVGEN STRUCT
        counter DWORD ?
        period DWORD ?
        attack DWORD ?
        halt DWORD ?
        step DWORD ?
        hold DWORD ?
        alternate DWORD ?
        output WORD ?
        maxStep db ?
    YM_ENVGEN ENDS

    YM_NOISEGEN STRUCT
        counter DWORD ?
        period DWORD ?
        lfsr DWORD ?
        output WORD ?
    YM_NOISEGEN ENDS

    SOFTWARE_EFFECT STRUCT
      num DWORD ?        ; 0 or 1
      typ DWORD ?
      param  DWORD ?
      currentValue  DWORD ?
      timerCount DWORD ?
      timerPeriod DWORD ?
      lpChannel DWORD ?
      lpChannelSticky DWORD ?
    SOFTWARE_EFFECT ENDS

    chnA YM_CHANNEL <>
    chnB YM_CHANNEL <>
    chnC YM_CHANNEL <>
    enve YM_ENVGEN <>
    noise YM_NOISEGEN <>
    effect1 SOFTWARE_EFFECT <>
    effect2 SOFTWARE_EFFECT <>
    
;----------------------------------------------------------------------------------------

.code

; Read one byte of data from [src], AND it with the mask, and store the result in [dest]
COPY_BYTE MACRO dest,src,mask,shcount
    mov al,[src]
    and al,mask
    shr al,shcount
    mov [dest],al
ENDM

; Set property 'prop' on all three channels to the value 'val'
SET_ALL_CHANNEL_PROPERTY MACRO prop,val
    mov [chnA.&prop&],val
    mov [chnB.&prop&],val
    mov [chnC.&prop&],val
ENDM

 
UPDATE_COUNTER MACRO oscillator, delta, endLabel
    mov eax,[&oscillator&.counter]
    mov ebx,[&oscillator&.period]
    add eax,[delta]
    cmp eax,ebx
    mov [&oscillator&.counter],eax
    jl endLabel
    sub [&oscillator&.counter],ebx
ENDM

; Special case of UPDATE_COUNTER for the three square wave generators 
UPDATE_CHANNEL_COUNTER MACRO channel, delta
    LOCAL _no_phase_change
    mov eax,[&channel&.counter]
    add eax,delta 
    cmp eax,[&channel&.period]
    jl _no_phase_change
    sub eax,[&channel&.period]
    xor [&channel&.phase],1
_no_phase_change:
    mov [&channel&.counter],eax
ENDM

; dl is assumed to contain the value of [noise.output]
CALC_CHANNEL_OUTPUT MACRO channelName
    mov al,[chn&channelName&.phase]
    mov bl,[chn&channelName&.noise]
    or al,[chn&channelName&.tone]
    or bl,dl
    mov ecx,[chn&channelName&.lpCurVol]
    and bl,al
    movzx ecx,WORD PTR [ecx]
    neg bl
    movsx eax,bl
    and eax,ecx
    mov [out&channelName&],eax
ENDM

RESET_EFFECT MACRO eff,regNum,endLabel
    mov [&eff&.lpChannel],0
    movzx eax,[ymRegs + regNum]
    shr eax,4
    and eax,3
    test eax,eax
    jz endLabel
    dec eax
    imul ebx,eax,SIZEOF YM_CHANNEL
    add ebx,OFFSET chnA
    mov [&eff&.lpChannel],ebx
    cmp ebx,[&eff&.lpChannelSticky]
    mov [&eff&.lpChannelSticky],ebx
ENDM

;----------------------------------------------------------------------------------------

ym_emu_init PROTO STDCALL :DWORD
ym_emu_run PROTO STDCALL :DWORD,:DWORD
streamproc PROTO STDCALL :DWORD,:DWORD,:DWORD,:DWORD
cleanup PROTO STDCALL

;----------------------------------------------------------------------------------------

start:

    invoke GetStdHandle,STD_OUTPUT_HANDLE
    mov [hConsole],eax
    invoke GetStdHandle,STD_INPUT_HANDLE
    mov [hConsoleInput],eax

    invoke CreateFile,ADDR szNameFile,
                      GENERIC_READ,
                      FILE_SHARE_READ,
                      NULL,OPEN_EXISTING,
                      FILE_ATTRIBUTE_NORMAL,
                      NULL
    cmp eax,INVALID_HANDLE_VALUE
    je quit
    mov [hFile], eax

    invoke GetFileSize,hFile,NULL
    mov [ymFileSize],eax

    invoke LocalAlloc, LMEM_FIXED, ymFileSize
    test eax,eax
    jz quit
    mov [lpFileData],eax

    invoke ReadFile, hFile, lpFileData, ymFileSize, ADDR bytesRead, NULL
    invoke CloseHandle, hFile

    invoke ym_emu_init, lpFileData

    invoke CreateEvent, 0, 0, 0, 0
    mov [hQuitEvent],eax

    invoke BASS_Init,-1, SAMPLE_RATE, BASS_DEVICE_MONO, 0, 0
    test eax,eax
    jz quit
    mov [bassState],STATE_INITED

    invoke BASS_StreamCreate, SAMPLE_RATE, 1, BASS_STREAM_AUTOFREE, streamproc, 0
    test eax,eax
    jz quit
    mov [hStream],eax

    invoke BASS_ChannelSetAttribute, hStream, BASS_ATTRIB_VOL, masterVolume

    invoke BASS_ChannelPlay, hStream, 0
    test eax,eax
    jz quit
    mov [bassState],STATE_STARTED

    invoke wsprintf, ADDR szBuffer, ADDR szInfoFormat, lpYmSongAuthor, lpYmSongTitle
    invoke WriteConsole, hConsole, ADDR szBuffer ,eax, ADDR bytesRead, NULL

    invoke WaitForSingleObject, hQuitEvent, -1

    quit:
    invoke cleanup
    invoke ExitProcess, 0

;----------------------------------------------------------------------------------------

cleanup PROC
    cmp [bassState],STATE_STARTED
    jne @F
    invoke BASS_Stop
    mov [bassState],STATE_INITED
@@:

    cmp [bassState],STATE_INITED
    jne @F
    invoke BASS_Free
    mov [bassState],STATE_UNDEFINED
@@:

    cmp [lpFileData],0
    je @F
    invoke LocalFree, lpFileData
@@:
    ret
cleanup ENDP

streamproc PROC handle:DWORD, lpBuffer:LPVOID, bytesToWrite:DWORD, userData:DWORD
    mov eax,[bytesToWrite]
    shr eax,1
    invoke ym_emu_run,eax,lpBuffer

    invoke PeekConsoleInput, hConsoleInput, ADDR szBuffer, 1, ADDR bytesRead
    cmp [bytesRead],0
    je no_keypress_detected
    ; Consume the input event
    invoke ReadConsoleInput, hConsoleInput, ADDR szBuffer, 1, ADDR bytesRead
    cmp (INPUT_RECORD PTR szBuffer).EventType,KEY_EVENT
    jne no_keypress_detected
    cmp (KEY_EVENT_RECORD PTR szBuffer+2).bKeyDown,0
    je no_keypress_detected

    invoke SetEvent, hQuitEvent
    mov eax,BASS_STREAMPROC_END
    jmp @F

no_keypress_detected:
    mov eax,[bytesToWrite]

@@:
    ret 16
streamproc ENDP


set_enve_shape PROC enveShape:DWORD
    movzx eax,BYTE PTR [enve.maxStep]
    mov ecx,[enveShape]
    xor edx,edx
    push esi                    ; attack = 0
    mov [enve.step],eax
    test ecx,4
    mov esi,eax
    push ebx
    cmovnz edx,eax              ; if (enveShape & 4) attack = maxStep
    mov ebx,[lpYmEnvTable]
    mov [enve.attack],edx
    xor esi,edx
    movzx esi,WORD PTR [ebx + esi*2]
    test ecx,8
    mov [enve.output],si
    jz envshape_no_continue
    xor ebx,ebx
    mov edx,eax
    test ecx,1
    cmovz eax,ebx               ; eax = (enveShape & 1) ? maxStep : 0  
    test ecx,2
    cmovz edx,ebx               ; edx = (enveShape & 2) ? maxStep : 0
envshape_no_continue:
    mov [enve.hold],eax         ; hold = (enveShape & 8) ? ((enveShape & 1) ? maxStep : 0) : maxStep
    mov [enve.alternate],edx    ; alternate = (enveShape & 8) ? ((enveShape & 2) ? maxStep : 0) : attack
    pop ebx
    pop esi
    ret 4
set_enve_shape ENDP


set_level PROC lpChannel:DWORD,level:DWORD
    mov ecx,[lpChannel]
    test ecx,ecx
    ASSUME ecx:PTR YM_CHANNEL
    jz return
    push ebx
    mov eax,[level]
    push esi
    mov edx,eax
    and eax,10h                 ; eax = level & 10h
    and edx,0Fh                 ; edx = level & 0Fh
    mov [ecx].mode,al
    lea ebx,[ecx].volume
    test eax,eax
    lea esi,[enve.output]
    mov ax,[YM_VOL_TB + edx*2]  ; ax = YM_VOL_TB[level & 0Fh]
    cmovnz ebx,esi              ; ebx = mode ? &enve.output : &lpChannel->volume
    mov [ecx].volume,ax
    mov [ecx].lpCurVol,ebx
    mov al,[chnA.mode]
    pop esi
    or al,[chnB.mode]
    or al,[chnC.mode]
    pop ebx
    xor al,10h
    mov BYTE PTR [enve.halt],al
    ASSUME ecx:NOTHING
return:
    ret 8
set_level ENDP


calc_tone_noise_masks PROC lpEffect:DWORD
    mov al,[ymRegs + YM_MIXER]
    test al,1
    setnz [chnA.tone]
    test al,2
    setnz [chnB.tone]
    test al,4
    setnz [chnC.tone]
    test al,8
    setnz [chnA.noise]
    test al,16
    setnz [chnB.noise]
    test al,32
    setnz [chnC.noise]
    mov edx,[lpEffect]
    ASSUME edx:PTR SOFTWARE_EFFECT
    cmp [edx].typ,SFX_DIGI_DRUM
    je @F
    cmp [edx].typ,SFX_SINUS_SID
    jne return
@@:
    cmp [edx].timerPeriod,0
    je return
    mov eax,[edx].lpChannel
    test eax,eax
    jz return
    mov (YM_CHANNEL PTR [eax]).tone,1
    mov (YM_CHANNEL PTR [eax]).noise,1
return:
    ASSUME edx:NOTHING
    ret 4
calc_tone_noise_masks ENDP


step_sid_voice_effect:
    ASSUME edx:PTR SOFTWARE_EFFECT
    mov eax,[edx].param
    xor [edx].currentValue,eax
    pop ebx     ; return address
    push [edx].currentValue
    push [edx].lpChannel
    push ebx
    jmp set_level
    ASSUME edx:NOTHING


step_digidrum_effect:
    ASSUME edx:PTR SOFTWARE_EFFECT
    pop ebx                     ; return address
    mov eax,[edx].param
    mov ecx,[digiDrumLen + eax*4]
    cmp [edx].currentValue,ecx
    jge end_of_sample
    mov ecx,[digiDrumPtr + eax*4]
    add ecx,[edx].currentValue  ; ecx = digiDrumPtr[sample] + samplePos
    inc [edx].currentValue
    movzx eax,BYTE PTR [ecx]
    cmp [digiDrumFormat],DIGI_DRUM_U4
    jne convert_u8_to_u4
    and eax,0Fh
    push eax
    push [edx].lpChannel
    push ebx
    jmp set_level
convert_u8_to_u4:
    ; ToDo: handle S8 format samples?
    movzx eax,BYTE PTR [YM_DIGIDRUM_8TO4_TB + eax]
    push eax
    push [edx].lpChannel
    push ebx
    jmp set_level
end_of_sample:
    ; The end of this digidrum sample has been reached; stop the effect
    mov [edx].lpChannel,0
    mov [edx].lpChannelSticky,0
    mov [edx].timerPeriod,0
    mov [edx].typ,SFX_NONE
    push edx
    push ebx
    jmp calc_tone_noise_masks
    ret
    ASSUME edx:NOTHING


step_sinus_sid_effect:
    ASSUME edx:PTR SOFTWARE_EFFECT
    mov eax,[edx].param          ; sample
    mov ecx,[edx].currentValue   ; samplePos
    shl eax,4
    inc DWORD PTR [edx].currentValue
    and DWORD PTR [edx].currentValue,7
    movzx eax,WORD PTR [SINUS_SID_TB + eax + ecx*2]
    mov ecx,[edx].lpChannel
    ASSUME edx:NOTHING
    mov (YM_CHANNEL PTR [ecx]).volume,ax
    ; Force output of the 16-bit sample value (bypass volume table lookup)
    lea edx,(YM_CHANNEL PTR [ecx]).volume
    mov (YM_CHANNEL PTR [ecx]).lpCurVol,edx
    ret


step_sync_buzz_effect:
    ASSUME edx:PTR SOFTWARE_EFFECT
    pop ebx	; return address
    push [edx].param
    push ebx
    jmp set_enve_shape
    ASSUME edx:NOTHING
    

.data
align 16
step_effect_jmp_table dd OFFSET step_sid_voice_effect
    dd OFFSET step_digidrum_effect
    dd OFFSET step_sinus_sid_effect
    dd OFFSET step_sync_buzz_effect

.code

step_effect PROC lpEffect:DWORD
    ASSUME edx:PTR SOFTWARE_EFFECT
    mov edx,[lpEffect]
    cmp [edx].timerPeriod,0
    je return
    mov eax,[speedTimer1]
    mov ecx,[edx].timerPeriod
    add [edx].timerCount,eax
    cmp [edx].timerCount,ecx
    jl return   
    sub [edx].timerCount,ecx
    mov eax,[edx].typ
    cmp eax,SFX_SID_VOICE
    jl return
    cmp eax,SFX_SYNC_BUZZ
    jg return
    call DWORD PTR [step_effect_jmp_table + eax*4]
return:
    ASSUME edx:NOTHING
    ret 4
step_effect ENDP


start_effect PROC lpEffect:DWORD
    ASSUME edx:PTR SOFTWARE_EFFECT
    mov edx,[lpEffect]
    mov ecx,[edx].lpChannel
    test ecx,ecx
    jz disable_effect
    mov eax,(YM_CHANNEL PTR [ecx]).num
    movzx ecx,BYTE PTR [ymRegs + YM_LEVEL_A + eax]
    cmp [edx].typ,SFX_DIGI_DRUM
    je start_digidrum
    cmp [edx].typ,SFX_SINUS_SID
    je start_sinus_sid
    and ecx,0Fh
    cmp [edx].param,ecx
    je start_timer
    mov [edx].param,ecx
    mov [edx].currentValue,ecx
    jmp start_timer
start_digidrum:
    and ecx,1Fh
    mov [edx].param,ecx         ; sample = ymRegs[YM_LEVEL_A + lpEffect->lpChannel->num] & 1Fh
    mov [edx].currentValue,0    ; samplePos = 0
    jmp start_timer
start_sinus_sid:
    and ecx,0Fh
    mov [edx].param,ecx         
    mov [edx].currentValue,0    ; samplePos = 0
start_timer:
    mov ecx,[edx].num
    movzx eax,BYTE PTR [ymRegs + 6 + ecx*2]
    shr eax,5
    movzx ecx,BYTE PTR [ymRegs + 14 + ecx]
    mov eax,[TIMER_PRESCALER_TB + eax*4]
    mul ecx
    mov edx,[lpEffect]
    mov [edx].timerPeriod,eax
    jmp return
disable_effect:
    cmp [edx].typ,SFX_DIGI_DRUM
    je disable_digidrum
    mov [edx].lpChannelSticky,0
    mov [edx].timerPeriod,0
    mov [edx].typ,SFX_NONE
    jmp return
disable_digidrum:
    ; Don't stop digidrum effects until the end of the sample has been reached
    mov eax,[edx].lpChannelSticky
    mov [edx].lpChannel,eax
return:
    invoke calc_tone_noise_masks,lpEffect
    ASSUME edx:NOTHING
    ret 4
start_effect ENDP


; Initialize YM emulation
; @param lpData Pointer to the uncompressed YM data
;
ym_emu_init PROC lpData:DWORD
    LOCAL numDigiDrums:DWORD
    pusha
    
    mov edx,[lpData]
    mov [lpYmData],edx

    movzx eax,WORD PTR [edx + 14h]
    ror ax,8 ; byteswap
    mov [numDigiDrums],eax
    
    mov ecx,DIGI_DRUM_U8
    test BYTE PTR [edx + 13h],4
    jz check_s8
    mov ecx,DIGI_DRUM_U4
    jmp set_digidrum_format
check_s8:
    test BYTE PTR [edx + 13h],2
    jz set_digidrum_format
    mov ecx,DIGI_DRUM_S8
set_digidrum_format:
    mov [digiDrumFormat],ecx
    
    lea edi,[edx + 22h]
    xor ecx,ecx
parse_digidrums:
    cmp [numDigiDrums],0
    je done_parsing_digidrums
    mov ebx,[edi]
    bswap ebx
    add edi,4
    cmp ecx,MAX_DIGIDRUMS
    jge @F
    mov [digiDrumPtr + ecx*4],edi
    mov [digiDrumLen + ecx*4],ebx
    inc ecx
@@:
    add edi,ebx
    dec [numDigiDrums]
    jmp parse_digidrums
done_parsing_digidrums:

    cld
    mov [lpYmSongTitle],edi
    xor eax,eax
    mov ecx,512
    repne scasb
    mov [lpYmSongAuthor],edi
    mov ecx,512
    repne scasb
    mov [lpYmSongComment],edi
    mov ecx,512
    repne scasb

    mov [lpYmRegStream],edi
    
    mov [speedToneNoise],(64000000 / SAMPLE_RATE) * 256
    mov [speedEnve],(64000000 / SAMPLE_RATE) * 256
    mov [enve.maxStep],31
    mov [speedTimer1],(2457600 * TIMER_MULT) / SAMPLE_RATE
  
    mov [lpYmEnvTable],OFFSET YM2149_VOL_TB

    cmp BYTE PTR [edx + 17h],1Bh
    jne @F
    ; This is probably a ZX Spectrum tune with an AY clock of 1773400 Hz
    ; (0x001B0F58 big-endian). Adjust the emulation speed accordingly.
    mov [speedToneNoise], (56748800 / SAMPLE_RATE) * 256
    mov [speedEnve], (56748800 / SAMPLE_RATE) * 128
    mov [enve.maxStep],15
    mov [lpYmEnvTable],OFFSET YM_VOL_TB
@@:

    mov [frame],0
    mov [noise.lfsr],10000h
    mov [noise.output],0
    
    movzx eax,WORD PTR [edx + 0Eh]
    ror ax,8
    mov [numFrames],eax
    
    movzx eax,WORD PTR [edx + 1Ch]
    ror ax,8
    mov [loopOffs],eax
    movzx eax,WORD PTR [edx + 1Eh]
    ror ax,8
    mov [loopFrame],eax

    ; Initialize software effects
    mov effect1.num,0
    mov effect1.typ,SFX_NONE
    mov effect1.timerPeriod,0
    mov effect1.lpChannel,0

    mov effect2.num,1
    mov effect2.typ,SFX_NONE
    mov effect2.timerPeriod,0
    mov effect2.lpChannel,0

    mov edi,[lpYmRegStream]
    xor ecx,ecx
copy_regs:
    mov al,[edi]
    mov [ymRegs + ecx],al
    inc ecx
    add edi,[numFrames]
    cmp ecx,16
    jne copy_regs
    
    mov [enve.alternate],0
    mov [enve.attack],0
    mov [enve.hold],0
    mov [enve.halt],0

    mov chnA.num,0
    mov chnB.num,1
    mov chnC.num,2
    mov chnA.lpCurVol,OFFSET chnA.volume
    mov chnB.lpCurVol,OFFSET chnB.volume
    mov chnC.lpCurVol,OFFSET chnC.volume
    SET_ALL_CHANNEL_PROPERTY phase,0
    SET_ALL_CHANNEL_PROPERTY tone,1
    SET_ALL_CHANNEL_PROPERTY noise,1
    SET_ALL_CHANNEL_PROPERTY mode,0

    mov [sampleCount],YM_FRAME_SAMPLES
    
    popa
    ret 4
ym_emu_init ENDP


ym_emu_run PROC numSamples:DWORD, lpBuffer:DWORD
    LOCAL outA : DWORD
    LOCAL outB : DWORD
    LOCAL outC : DWORD

    pusha
    
    mov esi,[numSamples]
    mov edi,[lpBuffer]

generate_samples:
    cmp [sampleCount],YM_FRAME_SAMPLES
    jne no_reg_updates
    movzx eax,BYTE PTR [ymRegs + YM_ENVE_SHAPE]
    cmp al,0FFh
    je @F
    invoke set_enve_shape,eax
@@:

    COPY_BYTE [chnA.mode],[ymRegs + YM_LEVEL_A],10h,0
    COPY_BYTE [chnB.mode],[ymRegs + YM_LEVEL_B],10h,0
    COPY_BYTE [chnC.mode],[ymRegs + YM_LEVEL_C],10h,0

    COPY_BYTE [chnA.tone],[ymRegs + YM_MIXER],01h,0
    COPY_BYTE [chnB.tone],[ymRegs + YM_MIXER],02h,1
    COPY_BYTE [chnC.tone],[ymRegs + YM_MIXER],04h,2

    COPY_BYTE [chnA.noise],[ymRegs + YM_MIXER],08h,3
    COPY_BYTE [chnB.noise],[ymRegs + YM_MIXER],10h,4
    COPY_BYTE [chnC.noise],[ymRegs + YM_MIXER],20h,5

    movzx eax,BYTE PTR [ymRegs + YM_LEVEL_A]
    invoke set_level,ADDR chnA,eax
    movzx eax,BYTE PTR [ymRegs + YM_LEVEL_B]
    invoke set_level,ADDR chnB,eax
    movzx eax,BYTE PTR [ymRegs + YM_LEVEL_C]
    invoke set_level,ADDR chnC,eax

    mov ax,WORD PTR [ymRegs + YM_TONEGENL_A]
    and ax,0FFFh
    shl eax,16
    mov [chnA.period],eax

    mov ax,WORD PTR [ymRegs + YM_TONEGENL_B]
    and ax,0FFFh
    shl eax,16
    mov [chnB.period],eax

    mov ax,WORD PTR [ymRegs + YM_TONEGENL_C]
    and ax,0FFFh
    shl eax,16
    mov [chnC.period],eax

    movzx eax,BYTE PTR [ymRegs + YM_NOISE_CTRL]
    and al,1Fh
    shl eax,16
    mov [noise.period],eax

    mov ax,WORD PTR [ymRegs + YM_ENVE_FREQL]
    shl eax,16
    mov [enve.period],eax

    lea ebx,[chnA.volume]
    cmp [chnA.mode],0
    lea ecx,[enve.output]
    cmovnz ebx,ecx
    mov [chnA.lpCurVol],ebx

    lea ebx,[chnB.volume]
    cmp [chnB.mode],0
    cmovnz ebx,ecx
    mov [chnB.lpCurVol],ebx

    lea ebx,[chnC.volume]
    cmp [chnC.mode],0
    cmovnz ebx,ecx
    mov [chnC.lpCurVol],ebx

    mov al,[chnA.mode]
    or al,[chnB.mode]
    or al,[chnC.mode]
    xor al,10h
    mov BYTE PTR [enve.halt],al

    mov eax,[lpYmData]
    cmp BYTE PTR [eax+2],'6'
    jne setup_ym5_effects
    ; YM6 supports 4 different effects; two of which can be active at the same time.
    ; r1[7:6] selects the type of effect to use for effect 1, and r1[5:4] selects the channel to apply it on.
    ; r3[7:6] selects the type of effect to use for effect 2, and r3[5:4] selects the channel to apply it on.
    RESET_EFFECT effect1,1,@F
    movzx eax,BYTE PTR [ymRegs + 1]
    shr eax,6
    and eax,3
    mov [effect1.typ],eax
@@:
    RESET_EFFECT effect2,3,@F
    movzx eax,BYTE PTR [ymRegs + 3]
    shr eax,6
    and eax,3
    mov [effect2.typ],eax
@@:
    jmp start_effects
setup_ym5_effects:
    RESET_EFFECT effect1,1,@F
    mov [effect1.typ],SFX_SID_VOICE
@@:
    RESET_EFFECT effect2,3,@F
    je same_channel
    cmp [effect2.typ],SFX_DIGI_DRUM
    jne same_channel
    invoke calc_tone_noise_masks, ADDR effect2
same_channel:
    mov [effect2.typ],SFX_DIGI_DRUM
@@:

start_effects:
    invoke start_effect, ADDR effect1
    invoke start_effect, ADDR effect2

no_reg_updates:

    mov edx,[speedToneNoise]
    UPDATE_CHANNEL_COUNTER chnA, edx
    UPDATE_CHANNEL_COUNTER chnB, edx
    UPDATE_CHANNEL_COUNTER chnC, edx

    cmp [enve.halt],0
    jne skip_envelope_update
    UPDATE_COUNTER enve,speedEnve,skip_envelope_update
    dec [enve.step]
    jns envelope_not_at_end
    mov eax,[enve.alternate]
    mov ebx,[enve.hold]
    movzx ecx,BYTE PTR [enve.maxStep]
    xor [enve.attack],eax	; attack ^= alternate
    xor ecx,ebx				
    mov [enve.halt],ebx		; halt = hold
    and [enve.step],ecx		; step &= (hold ^ maxStep)
envelope_not_at_end:
    mov eax,[enve.step]
    mov ecx,[lpYmEnvTable]
    xor eax,[enve.attack]
    mov bx,[ecx + eax*2]
    mov [enve.output],bx
skip_envelope_update:

    UPDATE_COUNTER noise,speedToneNoise,skip_noise_update
    mov eax,[noise.lfsr]
    mov ebx,eax
    mov ecx,eax
    and ebx,1
    shr eax,1				; eax = lfsr >> 1
    shr ecx,3
    mov [noise.output],bx	; output = lfsr & 1
    and ecx,1
    xor ebx,ecx
    shl ebx,16				; ebx = (output ^ ((lfsr >> 3) & 1)) << 16
    or eax,ebx
    mov [noise.lfsr],eax
skip_noise_update:

    invoke step_effect, ADDR effect1
    invoke step_effect, ADDR effect2

    mov dl,BYTE PTR [noise.output]
    CALC_CHANNEL_OUTPUT A
    CALC_CHANNEL_OUTPUT B
    CALC_CHANNEL_OUTPUT C

    ; eax is assumed to contain [outC]
    add eax,[outA]
    add eax,[outB]
    sub eax,11250

    ; Lowpass filter parameters taken from the Hatari emulator
    mov ebx,eax
    add ebx,[prevUnfiltered]
    cmp eax,[prevFiltered]
    mov [prevUnfiltered],eax
    jl @F
    lea ebx,[ebx*2 + ebx]
    mov ecx,[prevFiltered]
    lea ebx,[ebx + ecx*2]
    jmp output_sample
@@:
    mov ecx,[prevFiltered]
    lea ecx,[ecx*2 + ecx]
    lea ebx,[ebx + ecx*2]
output_sample:
    sar ebx,3
    mov [edi],bx
    mov [prevFiltered],ebx
    
    add edi,2

    cmp [sampleCount],0
    jne no_reg_load
    mov eax,[numFrames]
    inc DWORD PTR [frame]
    mov [sampleCount],YM_FRAME_SAMPLES
    cmp [frame],eax
    jl not_end_of_song
    mov [frame],0
not_end_of_song:
    mov ebx,[frame]
    mov edx,[lpYmRegStream]
    xor ecx,ecx
copy_regs:
    mov al,[edx + ebx]
    mov [ymRegs + ecx],al
    inc ecx
    add edx,[numFrames]
    cmp ecx,16
    jne copy_regs
    jmp reg_load_done
no_reg_load:
    dec DWORD PTR [sampleCount]
reg_load_done:

    dec esi
    jnz generate_samples
    
    popa
    ret 8
ym_emu_run ENDP


END start