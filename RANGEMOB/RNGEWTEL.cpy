      ******************************************************************
      *    NOME BOOK..:  RNGEWTEL                                      *
      *    DESCRICAO..:  LAYOUT PARA OS ARQUIVOS DE RANGE DE TELEFONES *
      *    DATA.......:  02/06/2023                                    *
      *    AUTOR......:  EDER GUIMARAES RODRIGUES                      *
      *    TAMANHO....:  71                                            *
      ******************************************************************
      *                                                                *
       05  RNGEWTEL-REGISTRO.
           10 RNGEWTEL-DATA.
              15 RNGEWTEL-TIPO-REGISTRO                PIC 9(01).
              15 RNGEWTEL-DDD                          PIC 9(02).
              15 RNGEWTEL-PREFIXO                      PIC 9(05).
              15 RNGEWTEL-RANGE-INICIAL                PIC 9(04).
              15 RNGEWTEL-RANGE-FINAL                  PIC 9(04).
              15 RNGEWTEL-EOT                          PIC X(03).
              15 RNGEWTEL-EOT-RECEPTORA                PIC X(03).
              15 RNGEWTEL-REGIAO                       PIC 9(04).
              15 RNGEWTEL-SETOR                        PIC 9(04).
              15 RNGEWTEL-UNIDADE-FEDERATIVA           PIC X(02).
              15 RNGEWTEL-AREA-LOCAL                   PIC X(04).
              15 RNGEWTEL-AREA-TARIFADA                PIC X(04).
              15 RNGEWTEL-LOCALIDADE-ID                PIC 9(05).
              15 RNGEWTEL-TIPO-PREFIXO                 PIC X(01).
              15 RNGEWTEL-PORTADO                      PIC X(01).
              15 RNGEWTEL-DATA-CADASTRO                PIC X(08).
              15 RNGEWTEL-DATA-INICIAL                 PIC X(08).
              15 RNGEWTEL-DATA-FINAL                   PIC X(08).
      *                                                                *
      ******************************************************************