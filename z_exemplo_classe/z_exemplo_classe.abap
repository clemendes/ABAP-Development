*****************************************************************************************
* NOME DO PROGRAMA    : Z_EXEMPLO_CLASSE                                         *                                                        *
* DESCRI��O           : Exemplo de Declara��o/Implementa��o de Classes / Objeto              *
*---------------------------------------------------------------------------------------*

REPORT Z_EXEMPLO_CLASSE.

*---------------------------------------------------------------------------------------*
* PAR�METROS DA TELA:                                                                   *
*   SELECT OPTIONS  (S_...)                                                             *
*   PARAMETERS      (P_...)                                                             *
*   CHECKBOX        (CB_...)                                                            *
*   RADIOBUTTON     (RB_...)                                                            *
*---------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: p_num TYPE i OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*---------------------------------------------------------------------------------------*
* RANGES                                                                                *
*   GLOBAL      (R_...)                                                                 *
*   LOCAL       (LR_...)                                                                *
*---------------------------------------------------------------------------------------*

*---------------------------------------------------------------------------------------*
* DEFINI��O DE CLASS  (LCL_...) - DECLARA��O                                                        *
*---------------------------------------------------------------------------------------*
CLASS lcl_teste DEFINITION.

  PUBLIC SECTION.     "Atributos vis�veis para todos
    METHODS:
      set_val IMPORTING VALUE(v_setval) TYPE i,
      get_val EXPORTING VALUE(v_getval) TYPE i,
      add_val.

  PROTECTED SECTION.  "Atributos que podem ser herdados por outra classa

  PRIVATE SECTION.    "Atributos visiveis apenas dentro da classe
    DATA cont TYPE i.

ENDCLASS.         "lcl_teste DEFINITION


*---------------------------------------------------------------------------------------*
* IMPLEMENTA��O DE CLASS  (LCL_...) - IMPLEMENTA��O                                                    *
*---------------------------------------------------------------------------------------*
CLASS lcl_teste IMPLEMENTATION.

  METHOD set_val.

    cont = v_setval.

  ENDMETHOD.

  METHOD get_val.

    v_getval = cont.

  ENDMETHOD.

  METHOD add_val.

    ADD 5 TO cont.

  ENDMETHOD.

ENDCLASS.         "lcl_teste IMPLEMENTATION

*---------------------------------------------------------------------------------------*
* OBJETOS                                                                               *
*   GLOBAL      (O_...)                                                                 *
*   LOCAL       (LO_...)                                                                *
*---------------------------------------------------------------------------------------*
DATA: obj_teste TYPE REF TO lcl_teste.

*---------------------------------------------------------------------------------------*
* VARI�VEIS                                                                             *
*   GLOBAIS     (V_...)                                                                 *
*   LOCAIS      (L_...)                                                                 *
*---------------------------------------------------------------------------------------*
DATA: v_valor TYPE i.

*---------------------------------------------------------------------------------------*
* Processamento e Exibi��o                                                              *
*---------------------------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM usar_objeto.
  WRITE: v_valor.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Form USAR_OBJETO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM usar_objeto .

  "O obj precisa ser 'iniciado' antes de utilizado
  CREATE OBJECT obj_teste.

  CALL METHOD obj_teste->set_val
    EXPORTING
      v_setval = v_valor.

  DO p_num TIMES.

    CALL METHOD obj_teste->add_val.

  ENDDO.

  CALL METHOD obj_teste->get_val
    IMPORTING
      v_getval = v_valor.

ENDFORM.