�aSIG: Free-DOS MODE.COM version 1.0.4, 19950805: English; Original              �.�>G u.�>K u.�>M u3��    .�Ku�.�>H t.�K��.�G�.�n�RQP�� �  �+.�Mu�.�>H t.�M�.�G�.�n�RQP�� �  ���.��XYZ.�G �Ϝ.�H���t���t���    .�H � .�6I.�K��.�6I.�M�؜��u<r��    �SP�����.�$X[� ���sj��seS��.��O��u�ÀtR��uN2�.����u��<��u���3KR�Ӏ���u���Āt�Àu�P��
����Ԩt�̀[��Z[��[��    ��=?�u$��KHuWVQ� ���G ��Y^_u�JM�ϝ�    MODE: Requires DOS version 2.0 or later
$�MODE: Insufficient memory
 �
MODE -- Free-DOS mode setting and general utility  Version 1.0.4, 19950805
	(c) Copyright 1994-1995 by K. Heidenstrom (kheidens@actrix.gen.nz)

MODE Videomode[,Lines]  - Select video mode and lines, Videomode may be:
		MONO		- 80-column monochrome (MDA and Hercules)
		BW40, BW80	- 40-column and 80-column color-suppressed CGA
		CO40, CO80	- 40-column and 80-column color
	Lines may be 25, 43, or 50 (43 requires EGA or VGA, 50 requires VGA)
MODE COMn:r,p,d,s  - Set serial port parameters (not permanent):
	n = port number (1-4)
	r = baud rate (50, 110, 150, 300, 600, 1200, 2400, 4800, 9600,
		14400, 19200, 28800, 38400, 57600, 115200, abbreviations)
	p = parity (e = even, n = none, o = odd)
	d = data bits (5-8)	s = stop bits (1-2)
MODE LPTn:P  - Infinite timeout on parallel port (1-4) (MODE will go resident)
MODE LPTn:=COMx  - Redirect printer output to COM port (MODE will go resident)
MODE LPTn:=NUL:  - Redirect printer output to NUL (MODE will go resident)
MODE LPTn:  - Remove redirection and infinite timeout on parallel port
MODE PARK[,minutes[:seconds]]  - Park now or after idle (MODE will go resident)
MODE DELAY=d RATE=r [LOCK]  - Set keyboard typematic delay and rate:
	d = delay (1-4)   r = rate (1-32)   LOCK = permanent (will go resident)
MODE SWITCHAR[=x]  - Report or set switchar
MODE CAPSLOCK=ON|OFF and/or NUMLOCK=ON|OFF and/or SCRLOCK=ON|OFF
 �MODE: Only one command allowed per invocation
 �MODE: Parameter out of range - check the help text
 �MODE: 43-line and 50-line modes are only usable with CO80 mode
 �MODE: Unknown baud rate specified
 �MODE: Must specify both DELAY and RATE for typematic setting
 sMODE: Specified serial port does not exist
 MODE: Serial port parameters set (not locked)
 MODE: LPT1 set to normal mode
 uMODE: No hard drive(s) found to park!
 MODE: Hard drive has been parked, switch off or press Ctrl-C to return to DOS
 MODE: Hard drives have been parked, switch off or press Ctrl-C to return to DOS
 MODE: Timed park function installed for one hard drive, MODE is resident
 MODE: Timed park function installed for two hard drives, MODE is resident
 MODE: Timed park timeout parameter updated in resident copy
 MODE: Timed park function enabled in resident copy for one hard drive
 MODE: Timed park function enabled in resident copy for two hard drives
 MODE: Typematic parameters set (not locked)
 MODE: Typematic parameters locked, resident portion of MODE installed
 MODE: Typematic parameters now locked by resident portion of MODE
 MODE: Locked typematic parameters updated
 vMODE: Specified video mode is not supported on this machine
 MODE: Parallel port set for infinite retry on timeout
 MODE: Parallel port redirected to NUL:
 MODE: Parallel port redirected to COM0
 MODE: Resident portion of MODE is now installed
 MODE: Switchar is currently '?'
 MODE: Switchar set to '?'
 �MODE: Switchar functions are not supported by the operating system
 mono bw40 �bw80 �co40 �co80 �com Elpt �park �delay= !rate= 1lock Eswitchar Ncapslock=off kcapslock=on vnumlock=off nnumlock=on yscrlock=off qscrlock=on |scrolllock=off qscrolllock=on | u;���� `      0   &  0  2  	9  `  n s  �  �  �    ,�� @ X� � �` `	0 � �% @8  K �p  �  �  � �� :=com �:=nul: �:=nul �:p �: �        �   �    �       �0�!<s7���	�!� �).�7C� X�L�!� ��C�� u���+�It�@�!ù ����; �sǼ� ST�D�?��KH�/��JMu�O��� �<t< v�N���������Q���� =�=�=�=��S��u7�<,u-F��- t- = t- = t��U��t
�>S�	u0��U�0����W��t�� �<:u������;w�t�H	���G�Y�� <nt<ot	<eu��[�[�i��8[�\��+����[��� ���\��uX��� ���@=�=��=� �^�� �<,u+F� 3�� �D��I�<:uF� <�� � ��I�>I t�� �� ���W��� �/��uz�� � ���.��uf��`�
�I�<=uF��� ��u��<tE�a��,�@=� =����@=� =���ċ�
Ċ&b
&c"�u	b��Q��t	:Qt�s�����3ۊ  :t2��u�GG�= u���u�C�9 u��Y�� ��u*�:�sû���3۬,0<
s���
 �����N�À<,u�Fø �<u$S2���s��BV[��2���s��BV�-� �����t�V���� u�V��� u�V�>Urw�Vu$�Vu�B����>Ss	�Vt���Vt��Vu3��ؠ$� .�>Su0��>Su�Vt��0�U�� ��S3���>U t�������B�� � ������ � ���� � ��:��3ێËW��&�� �һ�	tR�����Z�Y�B���BB�[��	�l�=��\�>O t�O�>^ u&ƇO  
�
�Ax�^��t��3��؋���һ�	t�&��&�u�5�!�����;�%�!�\�^��x&��O�&�O�^����x<��t�� �>O u�����Q�����+
s���I��u,�������T
B��r��������
���~���!���>O uS�K�M������
����r�����A�5�!�n�p�S�%�!�5�!�������%�!��� �O&�I&�K&�M&��&�t�����&��������r������5�!�n�p�S�%�!�5�!�������%�!��� �m	�.���t<�u�s��>O uR�>` u�\� �5�!�$�&��%�!�����e��/5�!�������/%�!�, �I�!�/ � 1�!�O&�.&�$&&u%�>` t��5�!�$�&��%�!����S�.��[���� L�!�a��u� 73��!r*��t&�Z�=�ڸ7�!� 73��!r:au	�v�_뽻{���b��3����6 ���렐