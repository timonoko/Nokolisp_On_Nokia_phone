�_~ xgrep  Version 1.03  Copyright (c) 1994 Robert Nordier @(#)xgrep.s 1.3 94/06/17 ��&�	����rQ3�� � 󫾀 ���$+�/-�~	��gtBrƇ�	��]
2� � �$���!��2���
���㋇6
�
�L
�&�	�F
������		�	8.�	t�,
8.�	t��	���G��� ���>r�;�v�OV����^r���	��	�@@:�u:�t:�u:�t:�t��:�u2䣴	�8u�� 3۴>�!N� ���'��N��+�V8.�	u1��u8.�	u�t�Ġ�	��t��	x:�u:�u��t �ࣴ	�� �J��+R�մ�!ZS��8.�	t>���s.�*.�2�Q��N�!Yr�Ft�~.t��O�\ �O뾴O�!s��VQ�Y^�-Q3ɴN�!Yr'���>�	UVRQS� =�!r(�3۴>�![YZ^]�O�!s�[��+��urR�մ�!Z���	8.�	uָ�
� ��3�9.�	u��	��
�t ^�t�.�	�����	�L�!�0�!<r93��, ����t.2�3�I�u�GG��� �� �� ��"
��<.t����-�X�. �"
�( �`��� �����P��t	� �.
� Z� �2
��2�3�I���ً,
���t� uN� tWQ���t��K+�AY_��\ :�t:�u��� t:�u� t���N2�N����6 t�֪<"t�i u��NËެ<"t��u�����;�v8G�tON��+ˋ��F���< s�Ë֋�2��0 t*</u�\<ar<zw$ߪ<*t<?u��<:t<\u�֋���O�Ūì< t<	t���Ë�V�v����u�^Ê��x)2�	t<�u��;�vK�?\t�C+���2�x
��yOO��	G�-�޸�	��u��	�ދ�3�� ��� r�OO��- t9���A��ЋȻ
2������8.�	t3��A�a�� �������Ju�3�3Ҿ ;�t���tЭ��t�3�+�������	��	8�	�u��u�
��	���I� ���<^u�<]t!��t[�!uFV���拴�	������&��^�$��<-u���t2<]t�� t��:�v�NN���� �<]u���,^u� ��0C��K���@�ø�	�� t���t(���؅�t"�À� ��	���� � t��3ҫ�«��3��â�	�������It�����Iu�J��� r=t>�ȋЬ<,u� r-uH�Ь<,t,�&�	��y<\u�<}t��y���t+�r뜰������������W��2����x��<\u	���x�����_�<:u��W�
�_�:ø ���*�A���������RS�޺ 
�,0r<	w����r�r	��N��+Ê�[Zà�	� t� ��	K��t�:C�\ ��	�@t7�F �NW�t�
 3ґ�����0O����u��_�t8�	}�:C�! ��	��t�֋�KK;�r�?
uKC� �2
�4
��+�v� �@�!r;�rË�؀?t��,
�w
� ��l
���3���	��	��	��	��	� -�6�	��	��	�6�	R��	� �+�u��+΁��w6� -��+�W��s��^� �S3۴?�![r��= �t1;�t)��O�=t��t��0���+�g;�tO�=
tG�
G����	��	Z���"8.�	t�8.�	}ý�	��	@u�F Ft�������	+�t԰
�u΋�+�It�}�u�E�I���s�AWV+�	v{��<tA<
t�8�umA�u�+�VQ�� � ���1�ׄ�t��FN+Mw4���������WSPVQ�۹ ��Y^X[_NIu��ˋ�8�	uADD��<
t�;�r��Y^��	��sF�3�^_��	t.2�u�D��&�	�8.�	}WRS��	���[Z_�*���	u��	����	���RP��3ҹ����XZ+�� chlnsvoxrydi            ^$.[*+?�  
                        Aa      
0  ^!  a  
0Aa  !:[{  	   _   A  
0Aa  �   �	�	�	�	�	�	�	�	�	�	�	�	zxutspnlgdcaXGREP      :   
  �
�
�
�
�
�
2xgrep Not enough space Invalid option Read error Write error Not found Can't open RE error NULL expression [  ] imbalance Range endpoint too large Bad number Range endpoint is zero More than 2 numbers given in \{  \} } expected after \ First number exceeds second in \{  \} Usage:   [-chlnsvyorx] expression [file ...] 
Options:
	-c  Line count only	-r  Recursively search subdirectories
	-h  No file names	-s  Silent about inaccessible files
	-l  File names only	-v  Non-matching lines
	-n  Number lines	-x  No magic: all operators need \
	-o  Errors to stdout	-y  Case-insensitive match

Syntax:
	^    start of line     repeat:  *	 0 or more times
	$    end of line		+	 1 or more times
	.    wildcard			?	 0 or 1 times
	[]   character class		\{m\}	 m times
	[^]  inverse class		\{m,\}	 m or more times
	\    quote next character	\{m,u\}	 m to u times

Classes:
	:a  alpha	:c  cntrl	:d  digit	:g  graph
	:l  lower	:n  alnum	:p  punct	:s  space
	:t  print	:u  upper	:x  xdigit	:z  ascii 