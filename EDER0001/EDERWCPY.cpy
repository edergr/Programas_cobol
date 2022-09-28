      ******************************************************************
      *    NOME BOOK..:  EDERWCPY                                      *
      *    DESCRICAO..:  MOVIMENTACOES                                 *
      *    DATA.......:  14/10/2021                                    *
      *    AUTOR......:  EDER GUIMARAES RODRIGUES                      *
      *    TAMANHO....:  26                                            *
      ******************************************************************
      *                                                                *
      *    H-OBJETICO-ARQUIVO.....: OBJETIVO DO ARQUIVO                *
      *                                                                *
      *    E-DATA-MOVIMENTACAO....: DATA DA MOVIMENTACAO               *
      *    E-TIPO-MOVIMENTACAO....: TIPO DA MOVIMENTACAO - 'D' OU 'C'  *
      *    E-VALOR-MOVIMENTACAO...: VALOR DA MOVIMENTACAO              *
      *                                                                *
      ******************************************************************
      *                                                                *
       05  EDERWCPY-REGISTRO.
           10 EDERWCPY-HEADER.
              15 EDERWCPY-H-OBJETICO-ARQUIVO           PIC X(25).
              15 FILLER                                PIC X(01).
           10 EDERWCPY-REGISTROS             REDEFINES EDERWCPY-HEADER.
              15 EDERWCPY-E-DATA-MOVIMENTACAO          PIC X(10).
              15 EDERWCPY-E-TIPO-MOVIMENTACAO          PIC X(01).
              15 EDERWCPY-E-VALOR-MOVIMENTACAO         PIC 9(13)V99.
      *                                                                *
      ******************************************************************
