�Jb%4;�//012c23934;4;J3�5�6G7X7:8r89�9�94;�9�$                                           
    �J�J                                        \   l                                     AXBXCXDXSPBPSIDIDSESSSCSIPdescfg&.6>de�󬭪�������lmno�����蚂���������               BD=:FreeDOS Debug v 0.95c help screen
Altering memory:
compare      C range address		hex add/sub  H value1 value2
dump         D [range]			move         M range address
enter        E address [list]		search       S range list
fill         F range list

Assemble/Disassemble:
assemble     A [address]		unassemble   U [range]
80x86 mode   Mx (0..6, ? for query)	set FPU mode MC 387, MNC none, MC2 287

Program execution:
go           G [=address] [breakpts]	quit         Q
proceed      P [=address] [count]	trace        T [=address] [count]
register     R register [value]		all regs     R
input        I port			output       O port byte

Disk access:
set name     N [[drive:][path]progname [arglist]]
load program L [[drive:][path]progname [arglist]]
load         L address drive sector number
write prog.  W [[drive:][path]progname]
write        W address drive sector number
The extended memory commands have not been installed yet.
$^ Error
$�	,<|MXk|���Write protect error Unknown unit error Drive not ready Unknown command Data error (CRC) Seek error Unknown media type Sector not found Unknown error Write fault Read fault General failure  read  writ ing drive x 8086/88 x86  without coprocessor  with coprocessor  with 287  (unused) [needs x86][needs math coprocessor][obsolete]Divide error.
$Unexpected single-step interrupt.
$Unexpected breakpoint interrupt.
$Program terminated normally.
$Program has terminated.
$EXE and HEX files cannot be written.
$Writing $ bytes.
$Disk full.
$Error $ opening file.
$File not found.
$Path not found.
$Access denied.
$Insufficient memory.
$Error in create-psp call.
$no�������ln��������@7��?���������D��o�5��C����  ��
���w����	�
?��������������1��~�O���I��6����������y���EԵ;�m._��-��J��K��K2��N��I���bc��a/��`���b��`���a|��d���d1��c���d~��b��`���a/��bc��a/��a���d~��c���d1��d���`���c���b���a���`H��cJ��cJ��c���b���a|��^���w�1���2;������<��}���.���#����������������<:�AE��AE����HA�BB��B��B��������������AE���A����A����A����A����A����A����A����A����A����A����A����A����A����A����A�����A�@����A���A����C/��C���K��A�@���B����AE��AE���A�������A�BB��B��B����"�W��� A�BB��B��B���������A���B��������������4�2�AE���A����ϡ��7�l���`���	A`��AE��AE��AE��AE��AE��AE��AE����n��
��{��A�BB��B��B�����O�AE��AE��AE���AE��AE��AE���>����AE����A����AE���AE��AE���0�eB������ͫ�B�����>������C1�r�����A�BB��B��B����R�����PA�BB��B��B��������AE���B���By���A���A����C/��C���B���By���A���.��AE��Aa�AE��AE��AE��AE��Id�����A�� N� O�����D�G �Q��X�� |��� ��=�=��=���Oh���P��>C�#��u��"��ts�"q�t&�#��uZ�"q�t&��Dn�#�t��&Z�x�%��wu�%s�w(�&�w��#��uZ�"q�t&�"��ts�#��u��"��ts�#X�u�&�w��%s�w(�%��wu�&Z�x�"$�s��%&�v��$��vA�#X�u�!��s��$��v��$��v��%&�v��$?�u��#�t��F�F!�նFo�/���M��;H���:���+��x�*p��<u��������D������3���4	�D"D!�C�C��C�C��C�C��C�C���M������0M0�(�)�*G*�57����V��W��W6�W��X�X��1T��1���W�����=��������+P���Z���������l���EaG���!���!c�Ņ�	�Q#�}�����-��/9�!�o� ��m�	?�}Z��������.��Ȑ����b����F�ͯ����_�����Cʬ����\
�������:�:��==��"�/���*˓������z�����v�N���&�.�6�>�d�e�*˓�����w����I�m��*����<��4V��4���z���yy��y,��z`��y,��y���}��|{��|.��|���z`��y,��yy��z���yy��z��|���|.��|{��}��x���{���{G��z��x���{���{���{���z���y���5����~��~�����M������v�J��L�K�3"��3o�����=�����+���2���'�'��������+�.���O���[p�����+�+��(R�(S�@��@�������x�t�� � �� �� �  � � � � � AAA AAD AAM AAS ADC ADD +AND 9ARPL =BOUND ABSF EBSR IBSWAP NBT UBTC ^BTR gBTS pCALL zCBW %CDQ }CLC �CLD �CLI �CLTS �CMC �CMOVA �CMOVAE �CMOVB �CMOVBE �CMOVC �CMOVE �CMOVG �CMOVGE �CMOVL �CMOVLE �CMOVNA �CMOVNAE �CMOVNB �CMOVNBE �CMOVNC �CMOVNE �CMOVNG �CMOVNGE �CMOVNL �CMOVNLE �CMOVNO �CMOVNP �CMOVNS �CMOVNZ �CMOVO �CMOVP �CMOVPE �CMOVPO �CMOVS CMOVZ CMP CMPSB CMPSD CMPSW CMPXCHG CMPXCHG8B !CPUID �CS &CWD yCWDE )DAA ,DAS /DB 1DD 3DEC 9DIV �DS <DW >ENTER �ES BF2XM1 FFABS JFADD UFADDP aFBLD dFBSTP gFCHS kFCLEX pFCMOVA uFCMOVAE zFCMOVB FCMOVBE �FCMOVE �FCMOVNA �FCMOVNAE �FCMOVNB �FCMOVNBE �FCMOVNE �FCMOVNU �FCMOVNZ �FCMOVU �FCMOVZ �FCOM �FCOMI �FCOMIP �FCOMP �FCOMPP �FCOS �FDECSTP �FDISI �FDIV �FDIVP FDIVR FDIVRP FENI $FFREE \FIADD (FICOM -FICOMP FIDIV FIDIVR 2FILD �FIMUL 9FINCSTP =FINIT BFIST GFISTP �FISUB FISUBR NFLD XFLD1 tFLDCW xFLDENV `FLDL2E \FLDL2T hFLDLG2 lFLDLN2 dFLDPI pFLDZ {FMUL �FMULP lFNCLEX �FNDISI  FNENI >FNINIT uFNLDCW �FNOP �FNSAVE �FNSETPM �FNSTCW �FNSTENV �FNSTSW �FPATAN �FPREM �FPREM1 �FPTAN �FRNDINT �FRSTOR �FS �FSAVE �FSCALE �FSETPM �FSIN �FSINCOS �FSQRT �FST �FSTCW �FSTENV �FSTP �FSTSW �FSUB �FSUBP FSUBR FSUBRP FTST FUCOM 'FUCOMI 0FUCOMIP 9FUCOMP BFUCOMPP GFWAIT JFXAM NFXCH UFXTRACT YFYL2X ]FYL2XP1 �GS aHLT dIDIV gIMUL yIN ~INC �INSB �INSD �INSW �INT �INTO �INVD �INVLPG �IRET �IRETD �JA �JAE �JB �JBE �JC �JCXZ �JE �JECXZ �JG �JGE �JL �JLE ZJMP �JNA �JNAE �JNB �JNBE �JNC �JNE JNG JNGE JNL JNLE JNO $JNP *JNS 0JNZ 6JO <JP BJPE HJPO NJS TJZ eLAHF hLAR lLDS ~LEA �LEAVE sLES vLFS �LGDT zLGS �LIDT �LLDT �LMSW �LOCK �LODSB �LODSD �LODSW �LOOP �LOOPE �LOOPNE �LOOPNZ �LOOPZ �LSL oLSS �LTR �MOV �MOVSB �MOVSD �MOVSW �MOVSX �MOVZX �MUL �NEG NOP NOT OR ORG OUT OUTSB !OUTSD "OUTSW &POP 8POPA 7POPAD =POPF <POPFD @PUSH ZPUSHA YPUSHAD _PUSHF ^PUSHFD bRCL jRCR �RDMSR �REP �REPE �REPNE �RET �RETF rROL zROR �RSM �SAHF �SAL �SAR �SBB �SCASB �SCASD �SCASW �SEG �SETA �SETAE �SETB �SETBE �SETC �SETE SETG SETGE SETL SETLE SETNA SETNAE SETNB  SETNBE $SETNC (SETNE ,SETNG 0SETNGE 4SETNL 8SETNLE <SETNO @SETNP DSETNS HSETNZ LSETO PSETP TSETPE XSETPO \SETS `SETZ dSGDT �SHL lSHLD �SHR sSHRD hSIDT zSLDT ~SMSW �SS �STC �STD �STI �STOSB �STOSD �STOSW �STR �SUB �TEST �VERR �VERW �WAIT �WBINVD �WRMSR �XADD �XCHG �XLAT �XLATB �XOR  
!#%')+-0247:>@BDFIKMQUY]`cegilnpsvy|���������������������������� N@ B@ R  BJ JB rz ZT ZR � RZ R"     B \ p"   0 0 0 � t    
 ~ ZR  VZ  ZRP VZP N" N, * "  $ R r r r NH HN l` \P Z&b Z&r "N ,N T 6 2 < 4 8 : &P &" B( B. B" p b RZ" RZ. ^\ \^ � � � � � � �������



77777777777777777777777777777777



�a�Yssssssssssssssss |�������/���������������������9�oqqq uuusii��--/sll��
 

������ ������� ������� � 55555555x1x1x xzzzzz  UUUUz zU<<<<<<<<@@RR@@@@< <<ULULPR JJJJJJJJ@@@@   J JJ N N@@@@RR >>>>>>>>CC  CCCC> >>U ULP PPRR  LLLLLLLLFF FFFFL LLN3N3    RR   ""������""""""""""""""""----------------������������������'����'��"'��'""��%%%%%%%%55****3�W            ��������      ��������& & & & & &  �





�U
U
U
U
U
U
 ������ �����������������3, 4500����������	����C=H8    

?
?
uuuuu�u��?
?
?
?
?
?
?
m �f  
 �y�uuuu{���

�	�	"2��uuuuuuuuuuuuuuuu  ZZ��  �``����    M
HXhx����R[:$����f NNN����     �� ((y �	� �	 �	�0  � �& 
U
�  � �& 
U
�gm17�	�	  �gm17�	�	  �gm17��  �
  ����
��f f NN  �)�)'o'**    �)�)�&'�)  '�E6�,.=6�,.=�  ��Kwn��  ���]�em��v~vb�bb�b      �  ��  �  �b�b�b�b Le�b  6�,.=6    ,=.�  ��  [�U  �D5_5    ]�em��v~=   (4$E5�  ��E�L�        0Ue�b  ���
�  '�����4�����4���43  8@�&c'� �E'Ju7u7u7u7u7u70Z=XnaJa�`�`�aea�`�`�a\a~a�a�`�`�`�`�6�666�6�66	6�6�6�6�6C6=6H686n9N9�8�8�9f9�8�8�9^9|9�9�8�8�8�88�7�QO0�9�98�7sX`0�9�9�5�A�Ai7Z0�6 7�7�7�T0;0A0�7�78J8J�G@G@G@G@G@G@G@G@�O0`0Z0T0�Q�uTT/=zh5:1(Bd$�                         	
                                                                         !"#$%&'()*+,-./0123456789:;<=>?@ABCDE  FGHIJK LMNOPQRST  UVWXYZ[\     ]^_`abcde                                                fg          hijk l      m       n       o       pq  rs   t      uvwxy    z      {       "! #]�$_^0�[&N������ � � m � � � � o a � ` � � � ��s �0*��� (��	
        ! ���������1�0+*	 	   ( ) ��� � 0 1 ��Ȏ؎����5M$�&3�� �<t�<;t�<?t ,a�<w"�t���K�����Ǵ	��!뾴	���!��                       ������$&   �&''    a ��,i,i,i,�,n,},�,-------,-j-�-�-�-�-�-..#.A.P.v.}.�.�.�.�.�.�.�.�.�.
�����5~%<t�����g�i�&3��K�i�$�:��g�$� ��s<t�<;t���$ ��$  ��KV<ar<zw$ߪ�<t< t<	u��cNV� ���������ެ< u�;�rN;�t�|� u�VFFW��K)χ��_^t	�< u������;�u�^^�����ؓ�� Í���<�r�,�t,�HtHt<sB�$�� �� ���$��$^X�<t�<;t��P����$< u�^X�<t<;t�9���$��K�� �>�$ u�^,u�<t�i��������K����$��$�㋇�$��$�|<tO<"t<'t�&:�$2���$���Lw'���Ĭ<t:�t	����Lw��:�t��><t<,t�������ëêþ�K��)��>g��>g�c��6�$^�<t<;u��$��6�$^���K<t<;u�
W� 3��_N�%�߀} u[W��@� �_uO�u	�D$�<Rup���وM���$%��=FAu�D$�<Ru��E��$���$%��=PTu��w�뙀>�$ u'W��@� �_uV��d<:uX�\�!A)ˊ��$�l�^W��@� �_rh��]��s������C�5���w&t��r2��$ �+�<(u$��,0<w�E�<)t����,0<w��E���u� :Et�E< u�����xr6��M�]�U
����"<:u��n�M�M�]�U
� �� �<[u���<]t�NW��@� ��_rC��s����} u�nN<*uF�]�b���Q�} u�]�	�} u�]�C�8�%�����8<*tM]U
��H�"NW��@3��^_rԀ} uΈ]�	<-t�<+u�h��h��} u(�} t�= u��� ��Et@@��$�E������E
E�u �Eu�E:�w��W��$� �_uջ�j��$�E
Eu�E���E��} u�M��|�����u<��$�m���E<t�< u�<|�$
���A�E< u���8<|Y$
�<t8�t� Q�E���Y�e	:�u;E
u� 
]t
�ǀ�@��ɀ�}�MN� ��<t<;t<,u���Ks����������V3���$� �6�$�,�r<tw��$����$����N���3һM ��$��$=� w<�r��$�Ĭ��$��$��$�6�$�֊����C��K�< t:�=�t�<@r� �����&�$�����%�늇�$�t�с|�VZtɃ����[��=�u���$�t�u�t�6�$�<�t�����$< t!<�u	��$t����$$�=� w߿�� �u���$t��$=� wƿ��	 �u���$�t��$�����Gu��>g��$t�����$< t��g��$t�H��$u��$ t���$< t
�<dr��$ ��$�؁�@s= r$��$$8�$�$�����$���$�㋇��� tP��X
�$���$��$t
���$tO���$�t
�O� �w��$�t
��$���w��$�t�w�����>g�>�$s��$%u��$ t��$��$�p���K�����5�0�E$����$�!�E$��$��>�$��uB�>�$��Eu6��$��$,|-t;���$�]
�t	��$8�u:�$t��$< u���E<tr
��$��E< t<t	<uހ�$ ��$��$����Q%룰�}
 t��$ ��E�>�$��$�>�$ð	�b�g@@��$u��$ t@�3ɋE�U
)�ʈE���:�u
9�u�E���$�gCCC�>�$ rC�U
�E< t<
t<u�CCC��$ ��uͰ�E�E3�)�ʉE�U�>�$�X�����]�9�E<w�u��$<u�}�u�� ��!��E<r�<s��$�"�]����$��$:Et� �E,<s�����$ËE�����E� :�ub;E
u]��] �e�E�>�$À}	t���$ ��8��$t��$t0�&�$��<"t��$��$ X�@��}��}���3%�;EtX��À} t�8Et�X���u�t�E����$t
H��E$�	�$��$�>�$�<-tP,0<
XrP$�,A<X��<-t�? � �u4
�u2ì�/ ��	ѹ t�������� �ƀt:���u�ǀt���u�Àt�AAA�3�3���rì��r����u� �������������u��tB��tB��t	B��t���UË%��<Eu"P���d���W��@Q� ��Y_XuF�Ӏ�����u)�K��r��FF�����AVW�#_^󦟊T�&�u�VW�_^�tIQ����K���H�:��D��?�  ����=�  ����4�  �XP�$�:��G��S�H_YA��<u�k�փ�s������ ������m�֋ѡm���:���$�P���  �X�]2�G�  VW�!_^9�tP�  ��&�CX@��΀�9�r��)�AVW�D_^�m�PQ�Y� �X< r<~v�.&�C���� t�  ��F���6k�E�-V�^��R�ZN;�sF��K���BGG�]2�B�k��/
VW�_^����<t*R�W�Ϻ�K)�_��H�r�W�_�֎���B�f�ÇӋ����:������  �W���&��_���.�RS� [��K3ɀ>^ t3V��J�6c;6er��r�>^u�<
uF�^�ଢ^�6c^���!< t#<t<t��s���6r�A���IN���!���)�Q���Y��
��B�
��
CZ��K<t��t�у�� ��C��
��/��Q3ɋ���)�AQR��G�ρ�K_�Ã�tX3����t
��KQ�YHu������K��Y��K�Ó���x�v��&��K�u��<t���u�N�<t�v��ۉӰ̆��������K�u�N�|<t�x�p�G<�t�ۉӈ���>zqu<���6�KN�L<t(�x�@��3��u���������9�u�������R����[�����  ���)���3��������%����
t�%�������3�� �=��V� �B����^����� r��.Cu
��OMt� ��.Eu���XEu� =�� �!s� P��?� ���!r= uX=MZu �B3�3��!�����>�!�K�� ���!rK��+. =� r�� �9�A�6�&����6����6��6����Q�!��;�����P���!� ��� � � �>�  t� =�� �!rk��B3�3��!����� �� � �������uA�>79�w9� B3��!�6;������9�r�ǎ��)Ǳ����?�!;�u�u�>�!�� �@3������"%��;�!�;�
 �;� �����g�����������á�Y����蓴H�!r8�=�Y�, 3���3�� ��� ��)���H����!��r�H�!s�b�;�?�7�������� ��U�6 ��!rI�=�ڣ, .�7= r3����HH���  ����+. =� r�� �9�A�P���!ô	�a�!ô	�x�!�V<,t<tK <lt���^N��Q���Ӌ�����9�u��$����8�Y�VW��_^�s������4�^N�<tS<0r<6w	,0�[�]� <ct<nuk� <cud���	�\ ���	�&[��u<3t<2u���	<u=�\�&]þ��[< t��0�����>\ t���]:[t�������� <t�	t:`t<ar<zw$ߪ��� ��>�W��KN��<u���K�\ �  ���l � ����K_WG��<u�X�)�HH�ì<t�A	t�:_tN�)�!P�<t�*	t:`u�NX<uHì<t��	t�� �N�<ar<zw$ߪ�  �����3�������R������Z���#Q�6�� �� ��� �u �E��t	���u�� �@t$?�� ���=<�u� $�4�8ut<�s*��u<t<@r<�r�<t4�uF<@r<�rFFFF���̆��K���������K�������و3ҁ>zqu";�u��H9�u���q��� �z�M9zu
�fY���,��r������}����ظ %�!����ظ%�!����ظ%�!�S�
 ��� �� <u��N�%�߿�� ����Ku4�� ����<uS�����^[<t���f�����u�| �<FuKN�<t< t<	u>�W<u��%<t�N�%�߿J� �u��Jw�E���!���E�	����N���3����QR�OZY)с�KO)�r�W�e_A��SW�_��K��u;��QW���V���^_u#R�����:��D����
��@� � �U��!Z��YA�X�h��Q�� ��>zMu��Y��ô	�z�!���t����<u�o��s�����  ����B�q�oAQR��@ �D[Y�o��+�)�9�s���t8�&���ˎێӋ&3��s"<v����������K�	��������7������ r&��.Eu	��XEu���.Hu��EXu�	���!ô<3ɺ� �!s� P�	���!��= r3���K�t�'@'�����K����	���![�6;��� ��>�K u	;�Kr��K��@3Ҏ��!;�u���)�K���K uΞu���	��!�A�� �!�>�!�= �,t-= �>t%= �Pt= �atP�	��!X��K��C��	�!Ë΁�Ja��w	A� ��!���	���!�&5�| �>A t�����;��P�&�X� �=� �=  t���I�!��ø L�!��P�;�!�$%�F�!�#%.�B�!�&7+&9�&. ���X[YZ]]^_.�&�.�6�.�6�.�6���.�z�.��.��  .�� .�A .�|�).�z=�.�zM�.�zq�.��.��.��.��.�&��̎Լ�WVULLRQSP.�&7���#5�!�B�D�#%� �!�$5�!�F�H�$%� �!�P���!�ێÁ&���À>R u�J3��޾� �����PR�	��<�!ZX�Error in sequence of calls to hack.
$�R�B��R �J3��ǿ� ����ø  ��:���K��)щa�\ ��@� �!�r=�e)�t�<t<
t����g s�
��KrNO�6c�^���@� �Ϻ�J)��!��
�~J�!��
�!��J�À>^ u�ÿ�J�6c;6er� r�>^u�<
uF����PSQR���?3۹K��)�t�!r	�t������eZY[X�V���^��3��)�SRQ�K���BY���r
�$�׎�^��5�           ��� <tQ��<tGRS�� ��t�	������R��A���h��RSV�J;�s�B�����>�= u^[�u�ZX[��[� >�G��OX�G�G3�I�� �� <t�;����" ��s <=u���� �t�v�sÀ>s t�t���v���s �| À>| t��	���!�f�Q�B V�@<,t <lt^Y�uI�s���N���#R� ��Z9�w\��R� ��Z�NI�rI��Ê$�� ��su"W ��� �u.����狝�_F�� <:u��m V�� <:t^N��X���� �X �����K<t�<'t<"t�] �G��Ĭ:�t<t۪���:�t�� <u���U rƘ�3۬�K r���u�� �����������3 r������) r���u�������� r��б�� r���u)������P,0<	vXP ,a<w
DD��X��� <u����$�,A<v�N�< t�<	t��< t<	t
<,t<;t<=ÅD�D(F5F>FQFuFzF~F�F�F�F�F�F�F�FG4GDGXGwG�G�G�G�G�G�G1 3 DXCLSTCSDSESFSGSSS        !$          �G�A�A�A�A�A
	ALCLDLBLAHCHDHBHAXCXDXBXSPBPSIDIESCSSSDSFSGSSTMMCRDRTRBYWOWODWQWFLDOTBSHLONEFA����&.6>de���� 
RK�&�@��@  ��@  ��@��@���$  ��<�u��@ ����@� �أ�@=�r
��H � ������ �㋟p��ñ��:�$r��$�����sW���@�����@� �����À�@��$�<�r���,��à�$$��Ä�@u�@��t�>�@X�j���V������u�t��^��@  ^��7�< tc��@ �<@r7��@ <`s<Ps	����@t��@�&�@����Ā����p�؃���K@�����(r��Y@��� uO���Y@�< t�,�띠�@��"�@u�� � ��#� � ��u� ��#� ���t&�@W�ߡ�@�u��5�Ɲ�_��!�t,��@�� u�W�	 ���_u� �W��K�� �_��@녨t0��@�� u�$���@W��� ��t4AA�u���t)���$��t�W��@��#�. �u���@t侸��@_� V�^W�_����@�� t��@�u� t����� �u�����)��W��K�q��:��o�� ���@��~� �����K��@������K���  ��K)����_��@t��}� uF�NW��@��_���K+�Q�]Y��c��@4�uZ�>�@wS��K�>�@ |t- HH�4�T�:���@��=���@������A���@&�&�W�>�@ |t������������@�t�u��@= t
=� t=� u��@ ��À� | ���@t������tP��X������<�r�����@��@t��:���$$ǀ�@��@t� ��@��@  <��[�t{�����@��u��t(�BP����@u��@��BX����@���t�+��SI����t�D���@���$�t.��$�u����@< �+}�-�؈%G����+����@��]��<u	�[��&� P$<u���$X��u�@t�< }���-G������$$<uZ��$$<u��$�u����[EGG���]���$������$<tA�[EGG���&�$���t�*��2�Āt	�4��@t�8��]���[EGG��]���#<�s����<�r���@���@u@��$�m���$��@�@�� |t�EG�������@�à�@�ڰ �ָQ � �FL��O��AT��DO��UB��LE���+�T���<�s��&�@�����t�����FA��R ��&�@��<�s�����dP�t�[P�W�L�:��tX�@X�<��-��'�:�{tP���Z�*�o�@�  ���o�@�X��$$<t����5���$$��ST��(��0)ثûCR�� <wu��$�>�@u$<u �n�� �DR����� <r\<s��$�TR���0��� <sB<t�&�@�<r��$���o< ��+}�۰-����]�l�t
�����@X�1��&�@��@  ��@  ��@���$  �&�@��$�A�
 �t��@���q �����@�	 )����r
��A� ����� ����� ���$���@@u��@@�� ��$��$ÿ�K��� ����Kr�à�@�������@�È&�@�>�@�� }	�BY��TE�t�D��WO��RD�� P��TR�� ��� P� �}X�yØ���P���@�X�þ��$t�D	7�]�>\ u�[:�$r�� ��D	6�[:�$s��$0�D� þ3�E�� �u��$)�3ɠ]:~�
 �V�6o.6�@�^��@�V�6o.6�@�^��@��6o��� Ku��6oÃ>�@ t�@��ϋρ�KW�NO�_�À�@��@þ�����K� �0 S�� [��K��# � ��2 � ���o������@�h����oËCC��=��Q�] Y�  ���Ë���I� �D�t�D �� �FF���   � @    NVUPDIPLNZNAPONCOVDNEINGZRACPECY���< u��P��� XP���� X$�'@'��P)�w� ��KY)�� ��O�= t�G�
��@� �Ϻ�K)��!�� �K0��N�����3�P�&3�J����!���ِZZ< <!<D$"DEBUG version 0.95c.  Debugger.

Usage:	DEBUG [[drive:][path]progname [arglist]]

  progname	(executable) file to debug or examine
  arglist	parameters given to program

For a list of debugging commands, run DEBUG and type ? at the prompt.
$Invalid switch - x
$�������� D3��!r�⁀��u�^ �, 3�� ����t����>Y� 0�!��=r��=�X��%�P��X% �= �tv�� �Q��X� ��[td�[�܃��f�fXf��f5   fPf�f�fXf;�t;�[f��f5    fPf�f�fXf;�t!fQf���f3��f��|f3�f@���$�[�fQf���[�]����JZZ�>�J��J< u4�>�J��J%?=? u%�\�>[u�������������>�J��J�u�]� 7�!�_��/u�`�� �< t�<	t�8�t"</u2�<?u,�D< t<	t<u�	��J�!� �<?t�K�	��K�!�L�!N��V� �}� 5�!�}��5�!�����5�!����� ��J���%��!���
 �S���D�D$�L�� ��L����