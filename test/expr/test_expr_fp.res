        ��  ��                  �      �� ��               <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
 <assemblyIdentity version="1.0.0.0" processorArchitecture="*" name="CompanyName.ProductName.AppName" type="win32"/>
 <description>Your application description.</description>
 <dependency>
  <dependentAssembly>
   <assemblyIdentity type="win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" processorArchitecture="*" publicKeyToken="6595b64144ccf1df" language="*"/>
  </dependentAssembly>
 </dependency>
 <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
  <security>
   <requestedPrivileges>
    <requestedExecutionLevel level="asInvoker" uiAccess="false"/>
   </requestedPrivileges>
  </security>
 </trustInfo>
 <compatibility xmlns="urn:schemas-microsoft-com:compatibility.v1">
  <application>
   <!-- Windows Vista -->
   <supportedOS Id="{e2011457-1546-43c5-a5fe-008deee3d3f0}" />
   <!-- Windows 7 -->
   <supportedOS Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}" />
   <!-- Windows 8 -->
   <supportedOS Id="{4a2f28e3-53b9-4441-ba9c-d69d4a4a6e38}" />
   <!-- Windows 8.1 -->
   <supportedOS Id="{1f676c76-80e1-4239-95bb-83d0f6d0da78}" />
   <!-- Windows 10 -->
   <supportedOS Id="{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}" />
   </application>
  </compatibility>
 <asmv3:application xmlns:asmv3="urn:schemas-microsoft-com:asm.v3">
  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2005/WindowsSettings">
   <dpiAware>True</dpiAware>
  </asmv3:windowsSettings>
  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">
   
   <longPathAware>false</longPathAware>
   
  </asmv3:windowsSettings>
 </asmv3:application>
</assembly>�  8   ��
 T E S T _ E X P R _ F P                   LAC    %COMMENTNESTED  TRUE  %AUTHOR  Duncan Munro  %CODEPREFIX  FP 
 %COPYRIGHT     %LICENCE     %TITLE 2 LaCoGen TEST Expression Evaluator (Floating Point) 
 %UNITLEXER     %UNITPARSER     %VERSION  0  %LEXERBUFFER  4096 	 %LEXERTAB  4  %PARSERBUFFER  1024  %START 	 <Command> LACM!	!
!! !(!)!*!+!,!-!.!/!0!1!2!3!4!5!6!7!8!9!=!?!A!B!C!D!E!F!G!H!I!J!K!L!M!N!O!P!Q!R!S!T!U!V!W!X!Y!Z!^!a!b!c!d!e!f!g!h!i!j!k!l!m!n!o!p!q!r!s!t!u!v!w!x!y!zLAC   1  (Error) 1  (EOF) 1	 (Comment) 1  number 1  id 1  help 1  print 1  let 1
 whitespace 1 	 <Command> 1  <PrintCommand> 1  <AssignCommand> 1  <HelpCommand> 1  <Expression> 1  = 1  + 1  <AddOp> 1  - 1  * 1  <MulOp> 1  / 1 	 <PowerOp> 1  ^ 1  ( 1  ) 1  , 1  <NumberVal> 1 	 <$accept> LAC   A���	









A   �������������������������������������������������������������������������A   �����������������������������������������������������������������������������A   �����������������������������������������������������������������������������A   �����������������������������������������������������������������������������A   �����������������������������������������������������������������������������A   �����������������������������������������������������������������������������A   �����������������������������������������������������������������������������A����������������������������������������������������������������������A   �����������������������������������������������������������������������������A   �����������









�����������������������������������������������������A   �����������������������������������������������������������������������������A   �����������������������������������������������������������������������������A   ���������������A   ���������������A   ���������������A   ���������������A   �����������������������������������������������������������������������������A   �����������������������������������������������������������������A��������������������������������������������������������������������A   ���������������A   ���������������A   ���������������A����������������������������������������������������������������������A   �������������������������������������������������������������������A   ���������������A   ���������������A   ���������������A   ���������������A   ���������������A   ���������������LAC   Q	       COMMAND_PRINTCOMMAND  <Command> : <PrintCommand> 
 procignore Q	       COMMAND_ASSIGNCOMMAND  <Command> : <AssignCommand> 
 procignore Q	       COMMAND_HELPCOMMAND  <Command> : <HelpCommand> 
 procignore Q
       PRINTCOMMAND_PRINT_EXPRESSION # <PrintCommand> : print <Expression> 	 procprint Q      ( ASSIGNCOMMAND_LET_ID__EQUALS__EXPRESSION ' <AssignCommand> : let id = <Expression>  proclet2 Q      $ ASSIGNCOMMAND_ID__EQUALS__EXPRESSION # <AssignCommand> : id = <Expression>  proclet1 Q       HELPCOMMAND_HELP  <HelpCommand> : help  prochelp Q      " EXPRESSION_EXPRESSION__PLUS__ADDOP % <Expression> : <Expression> + <AddOp>  procadd Q      $ EXPRESSION_EXPRESSION__HYPHEN__ADDOP % <Expression> : <Expression> - <AddOp>  procsub Q       EXPRESSION_ADDOP  <Expression> : <AddOp>  proccopy Q       ADDOP_ADDOP__ASTERISK__MULOP  <AddOp> : <AddOp> * <MulOp>  procmul Q       ADDOP_ADDOP__SLASH__MULOP  <AddOp> : <AddOp> / <MulOp>  procdiv Q       ADDOP_MULOP  <AddOp> : <MulOp>  proccopy Q       MULOP_POWEROP__CARET__MULOP  <MulOp> : <PowerOp> ^ <MulOp> 	 procpower Q       MULOP_POWEROP  <MulOp> : <PowerOp>  proccopy Q      ( POWEROP__LBRACKET__EXPRESSION__RBRACKET_  <PowerOp> : ( <Expression> )  procbrackets Q      + POWEROP_ID__LBRACKET__EXPRESSION__RBRACKET_  <PowerOp> : id ( <Expression> ) 	 procfunc1 Q      > POWEROP_ID__LBRACKET__EXPRESSION__COMMA__EXPRESSION__RBRACKET_ . <PowerOp> : id ( <Expression> , <Expression> ) 	 procfunc2 Q       POWEROP_NUMBERVAL  <PowerOp> : <NumberVal>  proccopy Q      
 POWEROP_ID  <PowerOp> : id  procid Q       NUMBERVAL__PLUS__NUMBER  <NumberVal> : + number  procunaryplus Q       NUMBERVAL__HYPHEN__NUMBER  <NumberVal> : - number  procunaryminus Q       NUMBERVAL_NUMBER  <NumberVal> : number 
 procnumber Q       _DOLLAR_ACCEPT_COMMAND___EOF__  <$accept> : <Command> (EOF) # PROC__DOLLAR_ACCEPT_COMMAND___EOF__ LAC-   a��������������������a���
������������	���a���������������������������a���������������������������a���������������������������a���������������������������a� ��������������������������a���������������������������a���������������������������a���
������������	���a������������������a���������������������������a���������������������������a��������������������a�����������������������a�	�������������	�	����		��a���������������������a��������������������a��������������������a���������������������������a���
�������� ����	���a����������������������������a����������������������!���a���
��������"����	���a��������������������a��������������������a���
����������#���	���a���
����������$���	���a���
������������%��	���a���
������������&��	���a���
������������'��	���a���
��������(����	���a�����������������������a��������������������a����������������������)*��a���������������������a���������������������a�
�������������
�

�
���

��a���������������������a���������������������a�����������������������a��������������������a���
��������+����	���a����������������������,���a��������������������