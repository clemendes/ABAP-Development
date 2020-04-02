*&---------------------------------------------------------------------&
*& Módule:      N/A (JOBs)
*&---------------------------------------------------------------------*
*& Description: Release control and scheduling of jobs by the user
*&---------------------------------------------------------------------*

REPORT ZCONTROL_JOBS.

INCLUDE <icon>.

TABLES: t100, ylibjobs, tbtco, sscrfields.

* TIPOS
TYPE-POOLS: VRM.
TYPES: BEGIN OF t_ylibjobs,
         job TYPE ylibjobs-job,
       END OF t_ylibjobs.

* TABELAS INTERNAS / ESTRUTURAS
DATA: it_ylibjobs TYPE STANDARD TABLE OF t_ylibjobs,
      wa_ylibjobs LIKE LINE OF it_ylibjobs,
      IT_STEPLIST TYPE MPD_TBTCSTEP_TAB.

* VARIAVEIS
DATA: NAME TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.
 DATA: GV_JOBNAME TYPE TBTCJOB-JOBNAME,
       GV_JOBNUM TYPE TBTCJOB-JOBCOUNT,
       GV_STATUS TYPE tbtco-status.
DATA: gv_erro TYPE abap_bool.

  SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
    PARAMETERS: P_JOB TYPE BTCJOB AS LISTBOX VISIBLE LENGTH 35.
  SELECTION-SCREEN END OF SCREEN 100.

  SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
    PARAMETERS: P_JOB2 TYPE BTCJOB AS LISTBOX VISIBLE LENGTH 35.
  SELECTION-SCREEN END OF SCREEN 200.

SELECTION-SCREEN BEGIN OF BLOCK b100 WITH FRAME TITLE text-001.
  SELECTION-SCREEN: BEGIN OF TABBED BLOCK mytab FOR 5 LINES,
                      TAB (20) button1 USER-COMMAND opc1,
                      TAB (20) button2 USER-COMMAND opc2,
                    END OF BLOCK mytab.
SELECTION-SCREEN END OF BLOCK b100.

INITIALIZATION.
  button1 = icon_led_yellow && 'ESCALONAR'(002).
  button2 = icon_led_green && 'LIBERAR'(003).
  mytab-prog = sy-repid.
  mytab-dynnr = 100.
  mytab-activetab = 'BUTTON1'.
  FREE MEMORY.
  PERFORM valida_acesso.
  PERFORM seleciona_liberados.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      CASE sscrfields-ucomm.
        WHEN 'OPC1'.
          mytab-dynnr = 100.
          mytab-activetab = 'BUTTON1'.
          CLEAR: p_job.
          PERFORM seleciona_liberados.
        WHEN 'OPC2'.
          mytab-dynnr = 200.
          mytab-activetab = 'BUTTON2'.
          CLEAR: p_job2.
          PERFORM seleciona_escalonados.
        WHEN OTHERS.
        ...
      ENDCASE.
      ...
  ENDCASE.

* INICIO
START-OF-SELECTION.
  CASE mytab-dynnr.
    WHEN 100.
      PERFORM processa_liberados. "OPCAO ESCALONAR
    WHEN 200.
      PERFORM processa_escalonados. "OPCAO LIBERAR
    WHEN OTHERS.
      ...
  ENDCASE.

****************************
* FORM VALIDA ACESSO TAB Z
****************************
FORM valida_acesso.

  REFRESH: it_ylibjobs[], list[].
  CLEAR: value.

* SELECIONA A LISTA DE JOBS PARA ESTE USUARIO
  SELECT job
    FROM ylibjobs
    INTO TABLE it_ylibjobs
    WHERE usuario EQ sy-uname.

  IF sy-subrc NE 0.
    gv_erro = abap_true.
    CONCATENATE 'Não existem Jobs cadastrado para o usuário '
                SY-UNAME INTO DATA(GV_MESSAGE) SEPARATED BY space.
    MESSAGE GV_MESSAGE TYPE 'W' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

****************************
* FORM SELECIONA LIBERADOS
****************************
FORM seleciona_liberados.

  REFRESH: it_ylibjobs[], list[].
  CLEAR: value.

    SELECT tbtco~jobname
    FROM tbtco
    JOIN ylibjobs
    ON tbtco~jobname EQ ylibjobs~job AND
       ylibjobs~usuario EQ @sy-uname AND
       tbtco~status EQ 'S'
    INTO TABLE @LIST.

     IF sy-subrc NE 0.
       EXIT.
     ELSE.
**         MONTA A LISTA PARA O MATCHCODE
*          LOOP AT it_ylibjobs INTO wa_ylibjobs.
*            value-key =  wa_ylibjobs-job.
*            value-text = wa_ylibjobs-job.
*            APPEND VALUE TO LIST.
*          ENDLOOP.
          NAME = 'P_JOB'.
          CALL FUNCTION 'VRM_SET_VALUES'
            EXPORTING
              ID     = NAME
              VALUES = LIST.
     ENDIF.

ENDFORM.

****************************
* FORM SELECIONA ESCALONADOS
****************************
FORM seleciona_escalonados.

  REFRESH: it_ylibjobs[], list[].
  CLEAR: value.

    SELECT tbtco~jobname
    FROM tbtco
    JOIN ylibjobs
    ON tbtco~jobname EQ ylibjobs~job AND
       ylibjobs~usuario EQ @sy-uname AND
       tbtco~status EQ 'P'
    INTO TABLE @LIST.

     IF sy-subrc NE 0.
       EXIT.
     ELSE.
**         MONTA A LISTA PARA O MATCHCODE
*          LOOP AT it_ylibjobs INTO wa_ylibjobs.
*            value-key =  wa_ylibjobs-job.
*            value-text = wa_ylibjobs-job.
*            APPEND VALUE TO LIST.
*          ENDLOOP.
          NAME = 'P_JOB2'.
          CALL FUNCTION 'VRM_SET_VALUES'
            EXPORTING
              ID     = NAME
              VALUES = LIST.
     ENDIF.

ENDFORM.

****************************
* FORM PROCESSA LIBERADOS
****************************
FORM processa_liberados.

  CHECK gv_erro EQ abap_false AND
        mytab-dynnr EQ 100 AND
        p_job NE space.

  SELECT jobcount
      FROM tbtco
      INTO GV_jobnum
      WHERE jobname EQ P_JOB AND
            status EQ 'S'. "procura por jobs liberados
      ENDSELECT.

     IF sy-subrc EQ 0.

       CALL FUNCTION 'BP_JOB_MODIFY'
        EXPORTING
          jobname              = p_job
          jobcount             = gv_jobnum
          dialog               = 'Y'
          opcode               = '18'
        TABLES
          new_steplist         = it_steplist
        EXCEPTIONS
          nothing_to_do        = 1
          OTHERS               = 99.

       WAIT UP TO 2 SECONDS.

         IF SY-SUBRC <> 0.
            MESSAGE 'Não foi possível escalonar o Job.' TYPE 'W' DISPLAY LIKE 'E'.
         ELSE.
            MESSAGE 'Job Escalonado!' TYPE 'I' DISPLAY LIKE 'S'.
         ENDIF.
     ELSE.
       gv_erro = abap_true.
       MESSAGE 'Job selecionado não se encontra com status liberado!' TYPE 'W' DISPLAY LIKE 'E'.
       CLEAR: P_JOB.
       EXIT.
     ENDIF.

ENDFORM.

****************************
* FORM PROCESSA ESCALONADOS
****************************
FORM processa_escalonados.

  CHECK gv_erro EQ abap_false AND
        mytab-dynnr EQ 200 AND
        p_job2 NE space.

  SELECT jobcount
      FROM tbtco
      INTO GV_jobnum
      WHERE jobname EQ P_JOB2 AND
            status EQ 'P'. "procura por jobs ecalonados
      ENDSELECT.

     IF sy-subrc EQ 0.

       CALL FUNCTION 'BP_JOB_RELEASE'
         EXPORTING
           JOBNAME                           = p_job2
           JOBCOUNT                          = gv_jobnum
         EXCEPTIONS
           MISSING_JOBNAME                   = 1
           MISSING_JOBCOUNT                  = 2
           MISSING_START_DATE                = 3
           STATUS_NOT_SCHEDULED              = 4
           CANT_ENQ_JOB                      = 5
           CANT_START_JOB_IMMEDIATELY        = 6
           NO_PRIVILEGE_TO_RELEASE_JOB       = 7
           CANT_RELEASE_JOB                  = 8
           JOB_NOT_EXIST                     = 9
           JOB_HAVE_NO_STEPS                 = 10
           ERROR_JOB_MODIFY                  = 11
           OTHERS                            = 12.

       WAIT UP TO 2 SECONDS.

         IF SY-SUBRC <> 0.
            MESSAGE 'Não foi possível liberar o Job.' TYPE 'W' DISPLAY LIKE 'E'.
         ELSE.
            MESSAGE 'Job Liberado!' TYPE 'I' DISPLAY LIKE 'S'.
         ENDIF.
     ELSE.
       gv_erro = abap_true.
       MESSAGE 'Job selecionado não se encontra com status escalonado!' TYPE 'W' DISPLAY LIKE 'E'.
       CLEAR: P_JOB2.
       EXIT.
     ENDIF.

ENDFORM.