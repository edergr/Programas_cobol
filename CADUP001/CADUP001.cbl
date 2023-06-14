      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. CADUP001.
      *AUTHOR.     EDER GUIMARAES RODRIGUES.
      *================================================================*
      *    PROGRAMA....:  CADUP001                                     *
      *    DATA........:  02/06/2023                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  RECEBE ARQUIVO CADUP E MONTA DOIS ARQUIVOS   *
      *                   SEPARANDO RANGES CORRENTES DE HISTORICOS     *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:  DDNAME                      BOOK'S           *
      *                  CADUPENT                    CADPWDAT          *
      *                  CADUPCOR                    CADPWDAT          *
      *                  CADUPHIS                    CADPWDAT          *
      *----------------------------------------------------------------*
      *    ARQ DESC....: CADUPENT - ARQUIVO DE ENTRADA COM TODOS OS    * 
      *                             DADOS DE RANGES DE OPERADORAS      *
      *                  CADUPCOR - ARQUIVO DE SAIDA APENAS COM REGIS- *
      *                             TROS CORRENTES                     *
      *                  CADUPHIS - ARQUIVO DE SAIDA APENAS COM REGIS- *
      *                             TROS DE HISTORICO                  *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *
           SELECT CADUPENT             ASSIGN
           TO '\home\ederrodrigues\Documentos\CADUPENT.dat'
           ORGANIZATION IS             LINE SEQUENTIAL
           FILE STATUS                 IS WRK-FS-CADUPENT.
      *
           SELECT CADUPCOR             ASSIGN
           TO '\home\ederrodrigues\Documentos\CADUPCOR.dat'
           ORGANIZATION IS             LINE SEQUENTIAL
           FILE STATUS                 IS WRK-FS-CADUPCOR.
      *
           SELECT CADUPHIS             ASSIGN
           TO '\home\ederrodrigues\Documentos\CADUPHIS.dat'
           ORGANIZATION IS             LINE SEQUENTIAL
           FILE STATUS                 IS WRK-FS-CADUPHIS.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *    INPUT......: ARQUIVO DE ENTRADA: MOVIMENTACOES              *
      *----------------------------------------------------------------*
       FD  CADUPENT
           RECORDING MODE IS F
           BLOCK CONTAINS  0 RECORDS.
      *
       01  FD-REG-CADUPENT             PIC  X(071).
      *----------------------------------------------------------------*
       FD  CADUPCOR
           RECORDING MODE IS F
           BLOCK CONTAINS  0 RECORDS.
      *
       01  FD-REG-CADUPCOR             PIC  X(071).
      *----------------------------------------------------------------*
       FD  CADUPHIS
           RECORDING MODE IS F
           BLOCK CONTAINS  0 RECORDS.
      *
       01  FD-REG-CADUPHIS             PIC  X(071).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** CADUP001 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*
       01  ACU-ACUMULADORES.
           05  ACU-REG-LIDOS           PIC  9(009) COMP-3 VALUE ZEROS.
           05  ACU-REG-GRAVADOS-CORR   PIC  9(009) COMP-3 VALUE ZEROS.
           05  ACU-REG-GRAVADOS-HIST   PIC  9(009) COMP-3 VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '* AREA DE TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*
       01  WRK-FILE-STATUS.
           05  WRK-OPERACAO            PIC  X(009) VALUE SPACES.
           05  WRK-ABERTURA            PIC  X(009) VALUE 'AO ABRIR '.
           05  WRK-LEITURA             PIC  X(009) VALUE 'AO LER   '.
           05  WRK-GRAVACAO            PIC  X(009) VALUE 'AO GRAVAR'.
           05  WRK-FECHAMENTO          PIC  X(009) VALUE 'AO FECHAR'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA PARA CAMPOS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-CAMPOS-AUXILIARES.
           05  WRK-PROGRAMA            PIC  X(008) VALUE 'CADUP001'.
           05  WRK-FS-CADUPENT         PIC  X(002) VALUE SPACES.
           05  WRK-FS-CADUPCOR         PIC  X(002) VALUE SPACES.
           05  WRK-FS-CADUPHIS         PIC  X(002) VALUE SPACES.
           05  WRK-TIPO-REGISTRO       PIC  X(001) VALUE SPACES.
           05  WRK-FLAG-ABEND          PIC  X(001) VALUE SPACES.
               88  WRK-ABENDAR                     VALUE 'S'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA PARA O BOOK DE ENTRADA E SAIDA ***'.
      *----------------------------------------------------------------*
       01  WRK-AREA-CADPWDAT.
           COPY CADPWDAT.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** CADUP001 - FIM DA AREA DE WORKING ***'.
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR.
           PERFORM 1100-TESTAR-FS.
           PERFORM 1200-VERIFICAR-VAZIO.
           PERFORM 1300-LER-CADUPENT.
           PERFORM 2000-PROCESSAR      UNTIL WRK-FS-CADUPENT EQUAL '10'.
           PERFORM 3000-FINALIZAR.
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE INICIALIZACAO DO PROGRAMA                         *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT CADUPENT.
               OUTPUT CADUPCOR CADUPHIS.
      *
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTAR O FS DOS ARQUIVOS                        *
      *----------------------------------------------------------------*
       1100-TESTAR-FS                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1110-TESTAR-FS-CADUPENT.
           PERFORM 1120-TESTAR-FS-CADUPCOR.
           PERFORM 1130-TESTAR-FS-CADUPHIS.
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE TESTE DE FILE-STATUS DO ARQUIVO CADUPENT          *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-CADUPENT         SECTION.
      *----------------------------------------------------------------*
      *
           IF (WRK-FS-CADUPENT         NOT EQUAL '00')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*      ERRO ' WRK-OPERACAO ' O ARQUIVO      *'.
               DISPLAY '*              CADUPENT              *'.
               DISPLAY '*          FILE STATUS = ' WRK-FS-CADUPENT
                                                 '          *'.
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               PERFORM 9000-PROCESSAR-TIPO-ERRO.
           END-IF.
      *
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------**
      *    ROTINA DE TESTE DE FILE-STATUS DO ARQUIVO CADUPCOR          *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-CADUPCOR         SECTION.
      *----------------------------------------------------------------*
      *
           IF (WRK-FS-CADUPCOR         NOT EQUAL '00')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*      ERRO ' WRK-OPERACAO ' O ARQUIVO      *'.
               DISPLAY '*              CADUPCOR              *'.
               DISPLAY '*          FILE STATUS = ' WRK-FS-CADUPCOR
                                                 '          *'.
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               PERFORM 9000-PROCESSAR-TIPO-ERRO.
           END-IF.
      *
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *    ROTINA DE TESTE DE FILE-STATUS DO ARQUIVO CADUPHIS          *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-CADUPHIS         SECTION.
      *----------------------------------------------------------------*
      *
           IF (WRK-FS-CADUPHIS         NOT EQUAL '00')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*      ERRO ' WRK-OPERACAO ' O ARQUIVO      *'.
               DISPLAY '*              CADUPHIS              *'.
               DISPLAY '*          FILE STATUS = ' WRK-FS-CADUPHIS
                                                 '          *'.
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               PERFORM 9000-PROCESSAR-TIPO-ERRO.
           END-IF.
      *
      *----------------------------------------------------------------*
       1130-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR SE EXISTEM REGISTROS NO ARQUIVO.      *
      *----------------------------------------------------------------*
       1200-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1300-LER-CADUPENT.
      *
           IF (WRK-FS-CADUPENT         EQUAL '10')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*                                    *'.
               DISPLAY '*     ARQUIVO CADUPENT ESTA VAZIO    *'.
               DISPLAY '*      PROCESSAMENTO ENCERRADO       *'.
               DISPLAY '*                                    *'.
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
           END-IF.
      *
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    LER ARQUIVO DE ENTRADA CADUPENT                             *
      *----------------------------------------------------------------*
       1300-LER-CADUPENT               SECTION.
      *----------------------------------------------------------------*
      *
           READ CADUPENT               INTO WRK-AREA-CADPWDAT.
      *
           IF  (WRK-FS-CADUPENT        EQUAL '10')
               CONTINUE.
           ELSE
               MOVE WRK-LEITURA        TO WRK-OPERACAO.
               PERFORM 1110-TESTAR-FS-CADUPENT.
               ADD  1                  TO ACU-REG-LIDOS.
           END-IF.
      *
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSOS                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2100-VERIFICA-REGISTRO.
      *     
           IF (WRK-TIPO-REGISTRO EQUAL 'C')
               PERFORM 2200-GRAVA-CORRENTE.
           ELSE
               PERFORM 2300-GRAVA-HISTORICO.
           END-IF.
      *     
           PERFORM 1300-LER-CADUPENT.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR SE O REGISTRO HISTORICO OU CORRENTE   *
      *----------------------------------------------------------------*
       2100-VERIFICA-REGISTRO          SECTION.
      *----------------------------------------------------------------*
      *
           IF (CADPWDAT-DATA-FINAL     EQUAL SPACES)
               MOVE 'C'                TO WRK-TIPO-REGISTRO
           ELSE
               MOVE 'H'                TO WRK-TIPO-REGISTRO
           END-IF
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR ARQUIVO CORRENTE                         *
      *----------------------------------------------------------------*
       2200-GRAVA-CORRENTE             SECTION.
      *----------------------------------------------------------------*
      *
           WRITE FD-REG-CADUPCOR       FROM WRK-AREA-CADPWDAT.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
      *
           PERFORM 1120-TESTAR-FS-CADUPCOR.
      *
           ADD  1                      TO ACU-REG-GRAVADOS-CORR.
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR ARQUIVO HISTORICO                        *
      *----------------------------------------------------------------*
       2300-GRAVA-HISTORICO            SECTION.
      *----------------------------------------------------------------*
      *
           WRITE FD-REG-CADUPHIS       FROM WRK-AREA-CADPWDAT.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
      *
           PERFORM 1130-TESTAR-FS-CADUPHIS.
      *
           ADD  1                      TO ACU-REG-GRAVADOS-HIST.
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA                           *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           CLOSE  CADUPENT.
      *
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
      *
           PERFORM 1100-TESTAR-FS.
           PERFORM 3100-EMITIR-ESTATISTICAS.
           PERFORM 3200-TERMINAR-PROCESSO.
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    EMITIR ESTATISTICAS                                         *
      *----------------------------------------------------------------*
       3100-EMITIR-ESTATISTICAS        SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY '******************** ' WRK-PROGRAMA
                  ' ********************'.
           DISPLAY '*         ESTATISTICAS DE PROCESSAMENTO          *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '* CADUP001 | I/O | DESC. ARQUIVO | QUANTID.      *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '* CADUPENT |  I  | TOTAL REG.    | ' ACU-REG-LIDOS
           '     *'.
           DISPLAY '* CADUPCOR |  O  | TOTAL REG.    | ' 
           ACU-REG-GRAVADOS-CORR '     *'.
           DISPLAY '* CADUPHIS |  O  | TOTAL REG.    | ' 
           ACU-REG-GRAVADOS-HIST '     *'.
           DISPLAY '******************** ' WRK-PROGRAMA
                  ' ********************'.
      *
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    TERMINAR PROGRAMA                                           *
      *----------------------------------------------------------------*
       3200-TERMINAR-PROCESSO          SECTION.
      *----------------------------------------------------------------*
      *
           STOP RUN.
      *
      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE VERIFICACAO DE ERROS                              *
      *----------------------------------------------------------------*
       9000-PROCESSAR-TIPO-ERRO        SECTION.
      *----------------------------------------------------------------*
      *
           MOVE 16                     TO RETURN-CODE.
           SET WRK-ABENDAR             TO TRUE.
           GOBACK.
      *
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*