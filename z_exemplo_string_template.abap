*&---------------------------------------------------------------------&
*& Program:     Z_EXEMPLO_STRING_TEMPLATE
*&---------------------------------------------------------------------*
*& Description: Exemplo de Utilização de String Templates (ABAP)
*&---------------------------------------------------------------------*

report z_exemplo_string_template.

DATA: lv_material type mara-matnr.

lv_material = '17'.
lv_material = |{ lv_material ALPHA = IN }|.

DATA(l_text1) = |Material: | && |{ lv_material ALPHA = IN }|.
DATA(l_text2) = |Material: | && |{ lv_material ALPHA = OUT }|.

* RESULTADO ESPERADO:
* l_text1: 000000000000000017
* l_text2: 17

BREAK-POINT.