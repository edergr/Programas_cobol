      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RANGEMOB.
      *AUTHOR.     EDER GUIMARAES RODRIGUES.
      *================================================================*
      *    PROGRAMA....:  RANGEMOB                                     *
      *    DATA........:  02/06/2023                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  RECEBE ARQUIVO DE RANGE DE TELEFONIA E MONTA *
      *                   DOIS NOVOS ARQUIVOS SEPARANDO CORRENTES DE   *
      *                   HISTORICOS                                   *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:  DDNAME                      BOOK'S           *
      *                  ARQRANGE                    RNGEWTEL          *
      *                  RNGECORR                    RNGEWTEL          *
      *                  RNGEHIST                    RNGEWTEL          *
      *----------------------------------------------------------------*
      *    ARQ DESC....: ARQRANGE - ARQUIVO DE ENTRADA COM TODOS OS    * 
      *                             DADOS DE RANGES DE OPERADORAS      *
      *                  RNGECORR - ARQUIVO DE SAIDA APENAS COM REGIS- *
      *                             TROS CORRENTES                     *
      *                  RNGEHIST - ARQUIVO DE SAIDA APENAS COM REGIS- *
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
           SELECT ARQRANGE             ASSIGN
           TO '\home\ederrodrigues\Documentos\ARQRANGE.dat'
           ORGANIZATION IS             LINE SEQUENTIAL
           FILE STATUS                 IS WRK-FS-ARQRANGE.
      *
           SELECT RNGECORR             ASSIGN
           TO '\home\ederrodrigues\Documentos\RNGECORR.dat'
           ORGANIZATION IS             LINE SEQUENTIAL
           FILE STATUS                 IS WRK-FS-RNGECORR.
      *
           SELECT RNGEHIST             ASSIGN
           TO '\home\ederrodrigues\Documentos\RNGEHIST.dat'
           ORGANIZATION IS             LINE SEQUENTIAL
           FILE STATUS                 IS WRK-FS-RNGEHIST.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *    INPUT......: ARQUIVO DE ENTRADA: MOVIMENTACOES              *
      *----------------------------------------------------------------*
       FD  ARQRANGE
           RECORDING MODE IS F
           BLOCK CONTAINS  0 RECORDS.
      *
       01  FD-REG-ARQRANGE             PIC  X(071).
      *----------------------------------------------------------------*
       FD  RNGECORR
           RECORDING MODE IS F
           BLOCK CONTAINS  0 RECORDS.
      *
       01  FD-REG-RNGECORR             PIC  X(071).
      *----------------------------------------------------------------*
       FD  RNGEHIST
           RECORDING MODE IS F
           BLOCK CONTAINS  0 RECORDS.
      *
       01  FD-REG-RNGEHIST             PIC  X(071).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** RANGEMOB - INICIO DA AREA DE WORKING ***'.
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
           05  WRK-PROGRAMA            PIC  X(008) VALUE 'RANGEMOB'.
           05  WRK-FS-ARQRANGE         PIC  X(002) VALUE SPACES.
           05  WRK-FS-RNGECORR         PIC  X(002) VALUE SPACES.
           05  WRK-FS-RNGEHIST         PIC  X(002) VALUE SPACES.
           05  WRK-TIPO-REGISTRO       PIC  X(001) VALUE SPACES.
           05  WRK-FLAG-ABEND          PIC  X(001) VALUE SPACES.
               88  WRK-ABENDAR                     VALUE 'S'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA PARA O BOOK DE ENTRADA E SAIDA ***'.
      *----------------------------------------------------------------*
       01  WRK-AREA-RNGEWTEL.
           COPY RNGEWTEL.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** RANGEMOB - FIM DA AREA DE WORKING ***'.
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
           PERFORM 1300-LER-ARQRANGE.
           PERFORM 2000-PROCESSAR      UNTIL WRK-FS-ARQRANGE EQUAL '10'.
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
           OPEN INPUT ARQRANGE.
               OUTPUT RNGECORR RNGEHIST.
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
           PERFORM 1110-TESTAR-FS-ARQRANGE.
           PERFORM 1120-TESTAR-FS-RNGECORR.
           PERFORM 1130-TESTAR-FS-RNGEHIST.
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE TESTE DE FILE-STATUS DO ARQUIVO ARQRANGE          *
      *----------------------------------------------------------------*
       1110-TESTAR-FS-ARQRANGE         SECTION.
      *----------------------------------------------------------------*
      *
           IF (WRK-FS-ARQRANGE         NOT EQUAL '00')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*      ERRO ' WRK-OPERACAO ' O ARQUIVO      *'.
               DISPLAY '*              ARQRANGE              *'.
               DISPLAY '*          FILE STATUS = ' WRK-FS-ARQRANGE
                                                 '          *'.
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               PERFORM 9000-PROCESSAR-TIPO-ERRO.
           END-IF.
      *
      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------**
      *    ROTINA DE TESTE DE FILE-STATUS DO ARQUIVO RNGECORR          *
      *----------------------------------------------------------------*
       1120-TESTAR-FS-RNGECORR         SECTION.
      *----------------------------------------------------------------*
      *
           IF (WRK-FS-RNGECORR         NOT EQUAL '00')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*      ERRO ' WRK-OPERACAO ' O ARQUIVO      *'.
               DISPLAY '*              RNGECORR              *'.
               DISPLAY '*          FILE STATUS = ' WRK-FS-RNGECORR
                                                 '          *'.
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               PERFORM 9000-PROCESSAR-TIPO-ERRO.
           END-IF.
      *
      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *    ROTINA DE TESTE DE FILE-STATUS DO ARQUIVO RNGEHIST          *
      *----------------------------------------------------------------*
       1130-TESTAR-FS-RNGEHIST         SECTION.
      *----------------------------------------------------------------*
      *
           IF (WRK-FS-RNGEHIST         NOT EQUAL '00')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*      ERRO ' WRK-OPERACAO ' O ARQUIVO      *'.
               DISPLAY '*              RNGEHIST              *'.
               DISPLAY '*          FILE STATUS = ' WRK-FS-RNGEHIST
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
           PERFORM 1300-LER-ARQRANGE.
      *
           IF (WRK-FS-ARQRANGE         EQUAL '10')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
               DISPLAY '*                                    *'.
               DISPLAY '*     ARQUIVO ARQRANGE ESTA VAZIO    *'.
               DISPLAY '*      PROCESSAMENTO ENCERRADO       *'.
               DISPLAY '*                                    *'.
               DISPLAY '************** ' WRK-PROGRAMA ' **************'.
           END-IF.
      *
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    LER ARQUIVO DE ENTRADA ARQRANGE                             *
      *----------------------------------------------------------------*
       1300-LER-ARQRANGE               SECTION.
      *----------------------------------------------------------------*
      *
           READ ARQRANGE               INTO WRK-AREA-RNGEWTEL.
      *
           IF  (WRK-FS-ARQRANGE        EQUAL '10')
               CONTINUE.
           ELSE
               MOVE WRK-LEITURA        TO WRK-OPERACAO.
               PERFORM 1110-TESTAR-FS-ARQRANGE.
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
           PERFORM 1300-LER-ARQRANGE.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR SE O REGISTRO HISTORICO OU CORRENTE   *
      *----------------------------------------------------------------*
       2100-VERIFICA-REGISTRO          SECTION.
      *----------------------------------------------------------------*
      *
           IF (RNGEWTEL-DATA-FINAL     EQUAL SPACES)
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
           WRITE FD-REG-RNGECORR       FROM WRK-AREA-RNGEWTEL.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
      *
           PERFORM 1120-TESTAR-FS-RNGECORR.
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
           WRITE FD-REG-RNGEHIST       FROM WRK-AREA-RNGEWTEL.
           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.
      *
           PERFORM 1130-TESTAR-FS-RNGEHIST.
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
           CLOSE  ARQRANGE.
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
           DISPLAY '* RANGEMOB | I/O | DESC. ARQUIVO | QUANTID.      *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '* ARQRANGE |  I  | TOTAL REG.    | ' ACU-REG-LIDOS
           '     *'.
           DISPLAY '* RNGECORR |  O  | TOTAL REG.    | ' 
           ACU-REG-GRAVADOS-CORR '     *'.
           DISPLAY '* RNGEHIST |  O  | TOTAL REG.    | ' 
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