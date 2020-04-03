report recursos_abap_740.

*- Declarações Inline
  DATA(lv_text) = 'Declaring variables'.
  DATA(lv_count) = 1000.

  BREAK-POINT.

*- Utilizando Value para criar tabela interna
  DATA lt_matnr TYPE STANDARD TABLE OF matnr.
  lt_matnr = VALUE #( ( 'a' ) ( 'b' ) ( 'c' ) ( 'd' ) ( 'e' ) ).

*- Outra forma de declaração do mesmo tipo
  TYPES:t_matnr TYPE STANDARD TABLE OF matnr WITH DEFAULT KEY .
  DATA(lt_data) = VALUE t_matnr( ( 'a' ) ( 'b' ) ( 'c' ) ( 'd' ) ( 'e' ) ).

*- Preenchendo dados dentro de uma tabela interna com estrutura
  DATA:lt_bukrs TYPE RANGE OF bukrs.
  lt_bukrs = VALUE #( sign = 'I'  option = 'EQ'
  ( low = '0001' )
  ( low = '1000' )
  ( low = '2000' )  ).

*- Declarando variavel work area para loop
  LOOP AT lt_matnr INTO DATA(ls_matnr).
  ENDLOOP.

*- Declarando variavel Field Symbol para loop
  LOOP AT lt_bukrs ASSIGNING FIELD-SYMBOL(<ls_bukrs>).
  ENDLOOP.

*- Declarando variavel Field Symbol para Read Table
  READ TABLE lt_matnr ASSIGNING FIELD-SYMBOL(<ls_matnr>) INDEX 3.
  IF sy-subrc EQ 0.
  ENDIF.

*- Outros exemplos de expressões tabelas
  DATA(wa_bukrs) = lt_bukrs[ low = '1000' ].

*- Checando se existe a linha
  IF line_exists( lt_bukrs[ low = '1000'] ).
  ENDIF.

*- Iteração com 'FOR'
  DATA(lt_material) = VALUE t_matnr( FOR ls_material IN lt_matnr ( ls_material ) ).

*- Outro uso do 'FOR'
  TYPES : BEGIN OF t_year ,
            year TYPE numc4,
          END OF t_year,
          tt_year TYPE STANDARD TABLE OF t_year WITH EMPTY KEY.
  DATA(lt_years)  = VALUE tt_year( FOR i = 2000 THEN i + 1 UNTIL i > 2020 ( year = i ) ).

*- Field Symbol com 'FOR'
  LOOP AT lt_years ASSIGNING FIELD-SYMBOL(<ls_year>).
    WRITE / <ls_year>-year.
  ENDLOOP.

*- Utilizando 'COND' para declarar novas variáveis
  DATA(lv_status) = 'S'.
  DATA(lv_status_text) =
    COND string(
      WHEN lv_status = 'A' THEN 'Aborted'
      WHEN lv_status = 'S' THEN 'Success'
      WHEN lv_status = 'F' THEN 'Failed'
      ELSE 'Status not found' ).

  WRITE / lv_status_text.

*- Podemos utilizar 'SWITCH' para declarar operadores condicionais
  DATA(lv_status_text2) = SWITCH string( lv_status
      WHEN 'A' THEN 'Aborted'
      WHEN 'S' THEN 'Success'
      WHEN 'F' THEN 'Failed'
      ELSE 'Status not found' ).

  WRITE / lv_status_text2.

*- CONCATENATE
  DATA(lv_time_string) = |Date: { sy-datum } / { sy-uzeit } Status: { lv_status_text } |.
  WRITE / lv_time_string.

*- ALPHA IN Line
  DATA:lv_ebeln TYPE ebeln VALUE '490001'.
  DATA(lv_purc_no) = |{ lv_ebeln ALPHA = IN } |.
  WRITE  / lv_purc_no.

*- CONVERSÃO IN Line
  DATA:lv_decimal TYPE p DECIMALS 10 VALUE '123.456789'.
  DATA(lv_money) = |{ lv_decimal DECIMALS = 2 } |.
  WRITE:lv_money.

*- Utilizando Case Condicional no acesso a tabela
  SELECT werks,
         CASE WHEN land1 = 'DE' THEN 'IN'
              ELSE 'OUT'
         END AS country
    FROM t001w INTO TABLE @DATA(lt_werks).

  LOOP AT lt_werks  ASSIGNING FIELD-SYMBOL(<ls_werks>).
    WRITE: / <ls_werks>-country,<ls_werks>-werks.
  ENDLOOP.

*- Preencher intervalos/tabelas diretamente select valores padrão
  DATA:lr_range TYPE RANGE OF matnr.

  SELECT matnr, 'ORHAN' AS yaratan
    FROM mara INTO TABLE
    @DATA(lt_mara) .

  SELECT 'I' AS sign ,'EQ' AS option, matnr AS low
      FROM mara
      INTO TABLE @lr_range UP TO 10 ROWS.