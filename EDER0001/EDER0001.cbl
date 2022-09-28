      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EDER0001.
      *AUTHOR.     EDER GUIMARAES RODRIGUES.
      *================================================================*
      *    PROGRAMA....:  EDER0001                                     *
      *    DATA........:  14/10/2021                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:  GERAR EXTRATO DE CONTA                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:  DDNAME                      BOOK'S           *
      *                  EDERARQE                    EDERWCPY          *
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
           SELECT EDERARQE             ASSIGN
           TO 'C:\Users\e3084\Desktop\SPARK\Cobol\EDER0001\EDERARQE.dat'
           ORGANISATION IS             LINE SEQUENTIAL
           FILE STATUS                 IS WRK-FS-EDERARQE.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *    INPUT......: ARQUIVO DE ENTRADA: MOVIMENTACOES              *
      *----------------------------------------------------------------*
       FD  EDERARQE
           RECORDING MODE IS F
           BLOCK CONTAINS  0 RECORDS.
      *
       01  FD-REG-EDERARQE             PIC  X(026).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** EDER0001 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*
       01  ACU-ACUMULADORES.
           05  ACU-REG-LIDOS           PIC  9(009) COMP-3 VALUE ZEROS.
           05  ACU-REG-HEADER          PIC  9(009) COMP-3 VALUE ZEROS.
           05  ACU-VALORES             PIC S9(013)V99
                                                   COMP-3 VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA PARA CHAVES DE QUEBRA ***'.
      *----------------------------------------------------------------*
       01  WRK-CHAVES.
           05  WRK-MES-ANTERIOR        PIC  9(002)        VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '* AREA DE TESTE DE FILE-STATUS *'.
      *----------------------------------------------------------------*
       01  WRK-FILE-STATUS.
           05  WRK-OPERACAO            PIC  X(009) VALUE SPACES.
           05  WRK-ABERTURA            PIC  X(009) VALUE 'AO ABRIR '.
           05  WRK-LEITURA             PIC  X(009) VALUE 'AO LER   '.
           05  WRK-FECHAMENTO          PIC  X(009) VALUE 'AO FECHAR'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA PARA CAMPOS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-CAMPOS-AUXILIARES.
           05  WRK-PROGRAMA            PIC  X(008) VALUE 'EDER0001'.
           05  WRK-FS-EDERARQE         PIC  X(002) VALUE SPACES.
           05  WRK-MASCARA             PIC  Z.ZZZ.ZZZ.ZZ9,99.
           05  WRK-SINAL               PIC  X(001) VALUE SPACES.
           05  WRK-MES-EXTRATO         PIC  X(009) VALUE SPACES.
           05  WRK-FLAG-ABEND          PIC  X(001) VALUE SPACES.
               88  WRK-ABENDAR                     VALUE 'S'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA PARA TRATAMENTO DE DATAS ***'.
      *----------------------------------------------------------------*
       01  WRK-DATA.
           05  WRK-DATA-X10            PIC  X(010) VALUE SPACES.
           05  WRK-DATA-X10-RDF        REDEFINES   WRK-DATA-X10.
               10  WRK-DATA-ANO        PIC  9(004).
               10  FILLER              PIC  X(001).
               10  WRK-DATA-MES        PIC  9(002).
               10  FILLER              PIC  X(001).
               10  WRK-DATA-DIA        PIC  9(002).
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** AREA PARA O BOOK DE ENTRADA ***'.
      *----------------------------------------------------------------*
       01  WRK-AREA-EDERWCPY.
           COPY EDERWCPY.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050) VALUE
           '*** EDER0001 - FIM DA AREA DE WORKING ***'.
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR.
           PERFORM 1200-VERIFICAR-VAZIO.
           PERFORM 1300-LER-EDERARQE.
           PERFORM 2000-PROCESSAR      UNTIL WRK-FS-EDERARQE EQUAL '10'.
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
           OPEN INPUT EDERARQE.
           MOVE WRK-ABERTURA           TO WRK-OPERACAO.
      *
           PERFORM 1100-TESTAR-FS-EDERARQE.
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE TESTE DE FILE-STATUS DO ARQUIVO EDERARQE          *
      *----------------------------------------------------------------*
       1100-TESTAR-FS-EDERARQE         SECTION.
      *----------------------------------------------------------------*
      *
           IF (WRK-FS-EDERARQE         NOT EQUAL '00')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'
               DISPLAY '*      ERRO ' WRK-OPERACAO ' O ARQUIVO      *'
               DISPLAY '*              EDERARQE              *'
               DISPLAY '*          FILE STATUS = ' WRK-FS-EDERARQE
                                                 '          *'
               DISPLAY '************** ' WRK-PROGRAMA ' **************'
               PERFORM 9000-PROCESSAR-TIPO-ERRO
           END-IF.
      *
      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR SE EXISTEM REGISTROS NO ARQUIVO.      *
      *----------------------------------------------------------------*
       1200-VERIFICAR-VAZIO            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1300-LER-EDERARQE.
      *
           IF (WRK-FS-EDERARQE         EQUAL '10')
               DISPLAY '************** ' WRK-PROGRAMA ' **************'
               DISPLAY '*                                    *'
               DISPLAY '*     ARQUIVO EDERARQE ESTA VAZIO    *'
               DISPLAY '*      ---> 0 MOVIMENTACOES <---     *'
               DISPLAY '*      PROCESSAMENTO ENCERRADO       *'
               DISPLAY '*                                    *'
               DISPLAY '************** ' WRK-PROGRAMA ' **************'
           ELSE
               ADD 1                   TO ACU-REG-HEADER
               IF (ACU-REG-LIDOS       EQUAL 1)
                   PERFORM 2100-IMPRIMIR-CABECALHO
               END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    LER ARQUIVO DE ENTRADA EDERARQE                             *
      *----------------------------------------------------------------*
       1300-LER-EDERARQE               SECTION.
      *----------------------------------------------------------------*
      *
           READ EDERARQE               INTO WRK-AREA-EDERWCPY.
           IF  (WRK-FS-EDERARQE        EQUAL '10')
               CONTINUE
           ELSE
               MOVE WRK-LEITURA        TO WRK-OPERACAO
               PERFORM 1100-TESTAR-FS-EDERARQE
               ADD  1                  TO ACU-REG-LIDOS
           END-IF.
      *
      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSOS                                         *
      *----------------------------------------------------------------*
       1400-VERIFICA-QUEBRA-MES        SECTION.
      *----------------------------------------------------------------*
      *
           MOVE EDERWCPY-E-DATA-MOVIMENTACAO
                                       TO WRK-DATA-X10.
      *
           IF  (WRK-DATA-MES           NOT EQUAL WRK-MES-ANTERIOR)
               PERFORM 2500-VERIFICAR-MES
               PERFORM 2600-IMPRIMI-QUEBRA-MES
               MOVE WRK-DATA-MES       TO WRK-MES-ANTERIOR
           END-IF.
      *
      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE PROCESSOS                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1400-VERIFICA-QUEBRA-MES.
           PERFORM 2200-GERAR-EXTRATO.
           PERFORM 2300-CALCULAR-EXTRATO.
           PERFORM 1300-LER-EDERARQE.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE IMPRESSAO DO CABECALHO                            *
      *----------------------------------------------------------------*
       2100-IMPRIMIR-CABECALHO         SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY '**************************************************'.
           DISPLAY '* EXTRATO ' EDERWCPY-H-OBJETICO-ARQUIVO
                   '              *'.
           DISPLAY '* ---------------------------------------------- *'.
           DISPLAY '* DATA       | MOVIMENTACAO | VALOR DA TRANSACAO *'.
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE IMPRESSAO DE LINHAS DE EXTRATO                    *
      *----------------------------------------------------------------*
       2200-GERAR-EXTRATO              SECTION.
      *----------------------------------------------------------------*
      *
           MOVE EDERWCPY-E-VALOR-MOVIMENTACAO
                                       TO WRK-MASCARA

           IF (EDERWCPY-E-TIPO-MOVIMENTACAO EQUAL 'D')
               MOVE '-'                TO WRK-SINAL
           ELSE
               MOVE SPACES             TO WRK-SINAL
           END-IF.
      *
           DISPLAY '* ' EDERWCPY-E-DATA-MOVIMENTACAO '   '
           EDERWCPY-E-TIPO-MOVIMENTACAO '              '
           WRK-MASCARA ' ' WRK-SINAL ' *'.
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA CALCULAR EXTRATO                                *
      *----------------------------------------------------------------*
       2300-CALCULAR-EXTRATO           SECTION.
      *----------------------------------------------------------------*
      *
           IF (EDERWCPY-E-TIPO-MOVIMENTACAO EQUAL 'D')
               SUBTRACT EDERWCPY-E-VALOR-MOVIMENTACAO
                                       FROM ACU-VALORES
           ELSE
               ADD EDERWCPY-E-VALOR-MOVIMENTACAO
                                       TO ACU-VALORES
           END-IF.
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE IMPRESSAO DE VALORES FINAIS DE EXTRATO            *
      *----------------------------------------------------------------*
       2400-IMPRIMIR-EXTRATO           SECTION.
      *----------------------------------------------------------------*
      *
           MOVE ACU-VALORES            TO WRK-MASCARA
           IF  (ACU-VALORES            GREATER THAN 0)
               MOVE '+'                TO WRK-SINAL
           ELSE
               MOVE '-'                TO WRK-SINAL
           END-IF.

           DISPLAY '* ---------------------------------------------- *'.
           DISPLAY '* SALDO FINAL                 ' WRK-MASCARA
           ' ' WRK-SINAL ' *'.
           DISPLAY '**************************************************'.
           DISPLAY ' '.
           DISPLAY ' '.
      *
      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA PARA VERIFICAR QUAL EH O MES CORRENTE                *
      *----------------------------------------------------------------*
       2500-VERIFICAR-MES              SECTION.
      *----------------------------------------------------------------*
      *
           EVALUATE WRK-DATA-MES
               WHEN 1
                   MOVE 'JANEIRO  '    TO WRK-MES-EXTRATO
               WHEN 2
                   MOVE 'FEVEREIRO'    TO WRK-MES-EXTRATO
               WHEN 3
                   MOVE 'MARCO    '    TO WRK-MES-EXTRATO
               WHEN 4
                   MOVE 'ABRIL    '    TO WRK-MES-EXTRATO
               WHEN 5
                   MOVE 'MAIO     '    TO WRK-MES-EXTRATO
               WHEN 6
                   MOVE 'JUNHO    '    TO WRK-MES-EXTRATO
               WHEN 7
                   MOVE 'JULHO    '    TO WRK-MES-EXTRATO
               WHEN 8
                   MOVE 'AGOSTO   '    TO WRK-MES-EXTRATO
               WHEN 9
                   MOVE 'SETEMBRO '    TO WRK-MES-EXTRATO
               WHEN 10
                   MOVE 'OUTUBRO  '    TO WRK-MES-EXTRATO
               WHEN 11
                   MOVE 'NOVEMBRO '    TO WRK-MES-EXTRATO
               WHEN 12
                   MOVE 'DEZEMBRO '    TO WRK-MES-EXTRATO
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE IMPRESSAO DA QUEBRA DE MES                        *
      *----------------------------------------------------------------*
       2600-IMPRIMI-QUEBRA-MES         SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY '* ---------------------------------------------- *'.
           DISPLAY '* ' WRK-MES-EXTRATO
           '                                      *'.
           DISPLAY '*                                                *'.
      *
      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DE FINALIZACAO DO PROGRAMA                           *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           CLOSE  EDERARQE.
      *
           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.
      *
           PERFORM 1100-TESTAR-FS-EDERARQE.
           PERFORM 2400-IMPRIMIR-EXTRATO.
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
           DISPLAY '* EDER0001 | I/O | DESC. ARQUIVO | QUANTID.      *'.
           DISPLAY '*------------------------------------------------*'.
           DISPLAY '* EDERARQE |  I  | TOTAL REG.    | ' ACU-REG-LIDOS
           '     *'.
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
           MOVE 16                     TO RETURN-CODE
           SET WRK-ABENDAR             TO TRUE
           GOBACK.
      *
      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
