      ******************************************************************
      *    NOME BOOK..:  CADPWDAT                                      *
      *    DESCRICAO..:  LAYOUT PARA OS ARQUIVOS CADUPS                *
      *    DATA.......:  02/06/2023                                    *
      *    AUTOR......:  EDER GUIMARAES RODRIGUES                      *
      *    TAMANHO....:  71                                            *
      ******************************************************************
      *                                                                *
      *    DATA-MOVIMENTACAO....: DATA DA MOVIMENTACAO                 *
      *    TIPO-MOVIMENTACAO....: TIPO DA MOVIMENTACAO - 'D' OU 'C'    *
      *    VALOR-MOVIMENTACAO...: VALOR DA MOVIMENTACAO                *
      *                                                                *
      ******************************************************************
      *                                                                *
       05  CADPWDAT-REGISTRO.
           10 CADPWDAT-DATA.
              15 CADPWDAT-TIPO-REGISTRO                PIC 9(01).
              15 CADPWDAT-DDD                          PIC 9(02).
              15 CADPWDAT-PREFIXO                      PIC 9(05).
              15 CADPWDAT-RANGE-INICIAL                PIC 9(04).
              15 CADPWDAT-RANGE-FINAL                  PIC 9(04).
              15 CADPWDAT-EOT                          PIC X(03).
              15 CADPWDAT-EOT-RECEPTORA                PIC X(03).
              15 CADPWDAT-REGIAO                       PIC 9(04).
              15 CADPWDAT-SETOR                        PIC 9(04).
              15 CADPWDAT-UNIDADE-FEDERATIVA           PIC X(02).
              15 CADPWDAT-AREA-LOCAL                   PIC X(04).
              15 CADPWDAT-AREA-TARIFADA                PIC X(04).
              15 CADPWDAT-LOCALIDADE-ID                PIC 9(05).
              15 CADPWDAT-TIPO-PREFIXO                 PIC X(01).
              15 CADPWDAT-PORTADO                      PIC X(01).
              15 CADPWDAT-DATA-CADASTRO                PIC X(08).
              15 CADPWDAT-DATA-INICIAL                 PIC X(08).
              15 CADPWDAT-DATA-FINAL                   PIC X(08).
      *                                                                *
      ******************************************************************