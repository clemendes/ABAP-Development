*&---------------------------------------------------------------------&
*& Módule:      Geral (ALV Report)
*&---------------------------------------------------------------------*
*& Description: 2 ALV's (Tables Head/Item) with Split Container
*               EXAMPLE >> LTAK + LTAP
*&---------------------------------------------------------------------*

REPORT z_two_alv_split_container.

* ----------------------------------------------------------
*                     DECLARAÇÕES                          *
* ----------------------------------------------------------
TABLES: LTAK.

TYPE-POOLS: abap.

DATA: gd_okcode TYPE ui_func,
      gd_repid       TYPE syrepid,
      go_docking     TYPE REF TO cl_gui_docking_container,
      go_docking2    TYPE REF TO cl_gui_docking_container,
      go_splitter    TYPE REF TO cl_gui_splitter_container,
      go_cell_top    TYPE REF TO cl_gui_container,
      go_cell_bottom TYPE REF TO cl_gui_container,
      go_grid1       TYPE REF TO cl_gui_alv_grid,
      go_grid2       TYPE REF TO cl_gui_alv_grid,
      gs_variant     TYPE disvariant,
      gs_layout      TYPE lvc_s_layo.

DATA: gs_LTAK TYPE LTAK,
      gt_LTAK TYPE STANDARD TABLE OF LTAK,
      gt_LTAP TYPE STANDARD TABLE OF LTAP.

* ----------------------------------------------------------
*                     PARAMETROS                           *
* ----------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE text-001.
  SELECT-OPTIONS : so_tanum  FOR LTAK-tanum, "Nº OT
                   so_lgnum  FOR LTAK-lgnum, "Nº Depósito
                   so_bdatu  FOR LTAK-bdatu, "Data Criação
                   so_tbnum  FOR LTAK-tbnum, "NT
                   so_kquit  FOR LTAK-kquit. "Confirmação
SELECTION-SCREEN END OF BLOCK part1.

* ----------------------------------------------------------
*             CLASSE LCL_EVENTHANDLER DEFINITION           *
* ----------------------------------------------------------
CLASS lcl_eventhandler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
     handle_double_click
          FOR EVENT double_click OF cl_gui_alv_grid
          IMPORTING
            e_row
            e_column
            es_row_no
            sender. "grid instance that has triggered the event

    CLASS-DATA: ms_row TYPE lvc_s_row READ-ONLY. " stores selected row

ENDCLASS. "lcl_eventhandler DEFINITION

* ----------------------------------------------------------
*            CLASSE LCL_EVENTHANDLER IMPLEMENTATION        *
* ----------------------------------------------------------
CLASS lcl_eventhandler IMPLEMENTATION.

  METHOD handle_double_click.
* DEFINE LOCAL VAR
    DATA: ls_LTAK TYPE LTAK.

    CHECK ( sender = go_grid1 ).

    READ TABLE gt_LTAK INTO ls_LTAK INDEX e_row-index.

    CHECK ( ls_LTAK-tanum IS NOT INITIAL ).

    lcl_eventhandler=>ms_row = e_row. " save selected row for REFRESH

* Triggers PAI of the dynpro with the specified ok-code
    CALL METHOD cl_gui_cfw=>set_new_ok_code('DETAIL').

  ENDMETHOD. "handle_double_click

ENDCLASS. "lcl_eventhandler IMPLEMENTATION

* ----------------------------------------------------------
*                   INICIO DO PROGRAMA                     *
* ----------------------------------------------------------
START-OF-SELECTION.

  REFRESH: gt_LTAK, gt_LTAP.
*
** SELECT PRINCIPAL 1º CONTAINER - CABEÇALHO
*  SELECT * FROM LTAK
*   INTO TABLE gt_LTAK.
**      WHERE ordem = .

  SELECT * FROM LTAK
    INTO TABLE gt_LTAK
      WHERE tanum IN so_tanum AND
            lgnum IN so_lgnum AND
            bdatu IN so_bdatu AND
            tbnum IN so_tbnum AND
            kquit IN so_kquit.

* Create the control and grid instances in advance* before displaying them on any screen
  PERFORM init_controls.
* Link the docking container to the target dynpro
* NOTE: Now we decide where to display the ALV list
  gd_repid  = syst-repid.

  CALL METHOD go_docking->link
  EXPORTING
    repid = gd_repid
    dynnr = '0100'
* CONTAINER =
  EXCEPTIONS
    OTHERS = 4.

* NOTE: dynpro does not contain any elements
  CALL SCREEN '0100'.
* Flow logic of dynpro (does not contain any dynpro elements):
END-OF-SELECTION.

*----------------------------------------------------------------------*
*  MODULE STATUS_0100 OUTPUT
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'STATUS_0100'. "contains push button “DETAIL”*
  SET TITLEBAR 'xxx'.

ENDMODULE. " STATUS_0100 OUTPUT

*----------------------------------------------------------------------*
*  MODULE USER_COMMAND_0100 INPUT
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  TRANSLATE gd_okcode TO UPPER CASE.

  CASE gd_okcode.
    WHEN 'BACK' OR 'END' OR 'CANC'.
      CASE syst-dynnr.
        WHEN '0100'.
          SET SCREEN 0.
          LEAVE SCREEN.
        WHEN '0200'.
          SET SCREEN 100.
          LEAVE SCREEN.
      ENDCASE.
** User has pushed button “Display Details”
    WHEN 'DETAIL'.
      PERFORM entry_show_details.
    WHEN OTHERS.
  ENDCASE.

  CLEAR: gd_okcode.

ENDMODULE. " USER_COMMAND_0100 INPUT

*&---------------------------------------------------------------------*
*&      Form  entry_show_details
*&---------------------------------------------------------------------*
FORM entry_show_details .
* define local data
  DATA: ld_row TYPE i,
         ls_LTAK TYPE LTAK,
         ls_stable TYPE lvc_s_stbl.

  REFRESH: gt_LTAP.

  READ TABLE gt_LTAK
  INTO ls_LTAK
  INDEX lcl_eventhandler=>ms_row-index.

  CHECK ( syst-subrc = 0 ).

* SELECT SECUNDARIO - DOUBLE CLICK (ALV ITENS)
  SELECT * FROM LTAP
  INTO TABLE gt_LTAP
     WHERE tanum = ls_LTAK-tanum AND
           lgnum = ls_ltak-lgnum.

 SORT gt_LTAP BY tapos ASCENDING.

* Refresh display of detail ALV list (reuired)
  ls_stable-row = abap_true.
  ls_stable-col = abap_true.

  CALL METHOD go_grid2->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      OTHERS    = 2.

  IF sy-subrc NE 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO*
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " ENTRY_SHOW_DETAILS

*&---------------------------------------------------------------------*
*&      Form  init_controls
*&---------------------------------------------------------------------*
FORM init_controls .

* Display both ALV grids on same main screen (splitter)
  PERFORM init_controls_main.

* Display data (before any screen is called !!!)
  gs_layout-grid_title = 'REGISTROS LTAK [TAB. CABEÇALHO]'.
  CALL METHOD go_grid1->set_table_for_first_display
    EXPORTING
      i_structure_name = 'LTAK'
      is_layout        = gs_layout
    CHANGING
      it_outtab        = gt_LTAK
    EXCEPTIONS
      OTHERS           = 4.

  IF sy-subrc NE 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO*
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  gs_layout-grid_title = 'REGISTROS LTAP [TAB. ITENS]'.

  CALL METHOD go_grid2->set_table_for_first_display
    EXPORTING
      i_structure_name = 'LTAP'
      is_layout        = gs_layout
    CHANGING
      it_outtab        = gt_LTAP " empty !!!
    EXCEPTIONS
      OTHERS           = 4.

  IF sy-subrc NE 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO*
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Set event handler
  SET HANDLER: lcl_eventhandler=>handle_double_click FOR go_grid1.

ENDFORM. " INIT_CONTROLS

*&---------------------------------------------------------------------*
*&      Form  init_controls_main
*&---------------------------------------------------------------------*
FORM init_controls_main .
* Create docking container
  CREATE OBJECT go_docking
  EXPORTING
  parent = cl_gui_container=>screen0
  ratio  = 90
  EXCEPTIONS
  OTHERS = 6.

  IF sy-subrc NE 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Fill entire screen
  go_docking->set_extension( 99999 ).
* Create splitter container
  CREATE OBJECT go_splitter
   EXPORTING
    parent            = go_docking
       rows              = 2
    columns           = 1
     EXCEPTIONS
    cntl_error        = 1
    cntl_system_error = 2
     OTHERS            = 3.

  IF sy-subrc NE 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Get cell container
  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = go_cell_top.

  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = go_cell_bottom.

* Create ALV grids
  CREATE OBJECT go_grid1
      EXPORTING
    i_parent = go_cell_top
   EXCEPTIONS
   OTHERS   = 5.

  IF sy-subrc NE 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Create 2nd grid instance for details of double-clicked* entry in first ALV grid
  CREATE OBJECT go_grid2
   EXPORTING
  i_parent = go_cell_bottom
  EXCEPTIONS
  OTHERS   = 5.

  IF sy-subrc NE 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM. " INIT_CONTROLS_MAIN
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2020. Sap Release 740