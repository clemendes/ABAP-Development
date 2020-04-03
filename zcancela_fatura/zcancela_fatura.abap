*&---------------------------------------------------------------------&
*& M�dule:      SD - Doc. Faturamento (SD-Billinng doc)
*&---------------------------------------------------------------------*
*& Description: Estorno Fatura VF11 - Reverse Doc. Billing
*&---------------------------------------------------------------------*

REPORT ZCANCELA_FATURA.

DATA: lv_doc LIKE BAPIVBRKSUCCESS-BILL_DOC,
      lv_date   TYPE bf_datm1eb.

DATA:
      t_return  TYPE STANDARD TABLE OF bapireturn1 WITH HEADER LINE,
      t_success TYPE STANDARD TABLE OF BAPIVBRKSUCCESS WITH HEADER LINE.

PARAMETERS:
      p_doc  like lv_doc, " N�mero da fatura SD.
      p_date like lv_date. " Data que contabilizar� o cancelamento.

START-OF-SELECTION.
  lv_doc = p_doc.
  lv_date = p_date.

* Func�o que estorna o documento
CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
  EXPORTING
    billingdocument = lv_doc
    billingdate  = lv_date
*  testrun     =
*  no_commit   =
  TABLES
    return  = t_return
    success = t_success.

*BREAK-POINT.
MESSAGE t_return-message TYPE 'I'.

* t_return retorna todas as mensagens ocorridas durante o cancelamento.
* t_succes retorna , entre outros dados, o n�mero do doc. cancelamento.