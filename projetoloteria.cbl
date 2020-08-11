      $set sourceformat"free"
      *> divis�o de identifica��o do programa
       identification division.
       program-id. "projetoloteria".
       author. "Jennyfer Aaujo".
       installation. "PC".
       date-written. 01/08/2020.
       date-compiled. 01/08/2020.

      *> divis�o para configura��o do ambiente
       environment Division.
       configuration Section.
           special-names. decimal-point is comma.

      *> declara��o dos recursos externos
       input-output Section.
       file-control.
       i-o-control.

      *> declara��o de vari�veis
       data division.

      *> vari�veis de arquivos
       file Section.

      *> vari�veis de trabalho
       working-storage section.

       01  aposta occurs 10.
           05  num                                 pic 9(02).

       01  sorteio-num occurs 6.
           05  sorteio                             pic 9(02).

       01  aposta-controle.
           05  numero-controle                     pic 9(02).
           05  sorteio-controle                    pic 9(02).
           05  controle-ctrl                       pic 9(01).

       77  controle-troca                          pic x(1).
           88  trocou                              value "t".
           88  nao_trocou                          value "n".

       77  controle                                pic 9(09).
       77  ind1                                    pic 9(02).
       77  ind2                                    pic 9(02).
       77  qnt_numero                              pic 9(02).
       77  semente                                 pic 9(08).
       77  num_random                              pic 9(02)V9999.

      *> variaveis para comunica��o entre programas
       linkage section.

      *> declara��o de tela
       screen section.

      *> declara��o do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

       inicializa section.

      *> iniciliza��o das vari�veis

           move 0 to controle
           move 0 to qnt_numero
           move 1 to ind1
           move 0 to controle-ctrl
           move 0 to num(1)
           move 0 to num(2)
           move 0 to num(3)
           move 0 to num(4)
           move 0 to num(5)
           move 0 to num(6)
           move 0 to num(7)
           move 0 to num(8)
           move 0 to num(9)
           move 0 to num(10)
           .

       inicializa-exit.
           exit.


       processamento section.

           *> vai performar at� que os n�meros escolhidos estejam entre 6 e 10
           perform until qnt_numero >= 6 and qnt_numero <= 10
               display "Quantos numeros vc quer apostar? (6 - 10)"
               accept qnt_numero
           end-perform

           perform varying ind1 from 1 by 1 until ind1 > qnt_numero
               display "Adicione o numero " ind1 " da aposta:"
               accept numero-controle

               perform foraintervalo

               move numero-controle to num(ind1)
               display erase
           end-perform

           perform until controle-ctrl = 6
               move 0 to sorteio(1)
               move 0 to sorteio(2)
               move 0 to sorteio(3)
               move 0 to sorteio(4)
               move 0 to sorteio(5)
               move 0 to sorteio(6)

               perform geradorrandom
               perform ganhador

               display "Tentativa numero: " controle
               accept numero-controle
           end-perform

           if controle-ctrl = 6 then
               display "Voce ganhou!"
           end-if
           .

       processamento-exit.
           exit.


       foraintervalo section.

               *> estrutura de decis�o para que a pessoa n�o adicione um n�mero fora do intervalo de 1 a 60
               if numero-controle > 60 or numero-controle < 1 then
                   display "Numero fora do intervalo"
                   display "Informe outro numero:"
                   accept numero-controle
               end-if
           .

       foraintervalo-exit.
       exit.


       geradorrandom section.

           perform varying ind2 from 1 by 1 until ind2 > 6
               accept semente from time
               compute num_random = function random(semente)
               multiply num_random by 60 giving sorteio-controle
               perform repeticao-sorteio
               move sorteio-controle to sorteio(ind2)
           end-perform

           add 1 to controle.
           .
       geradorrandom-exit.
           exit.


       repeticao-sorteio section.

      *> teste de repeti��o do sorteio com o m�todo bolha
           set nao_trocou to true
           perform varying ind2 from 1 by 1 until sorteio(ind2) = 0 or trocou
               if sorteio-controle = sorteio(ind2) then
                   compute ind2 = ind2 - 1
                   set trocou to true
               end-if
           end-perform
           .
       repeticao-sorteio-exit.
           exit.

       ganhador section.

           move 0 to controle-ctrl
           perform varying ind2 from 1 by 1 until ind2 > 6
               perform varying ind1 from 1 by 1 until ind1 > qnt_numero
                   if sorteio(ind2) = aposta(ind1) then
                       add 1 to controle-ctrl
                   end-if
               end-perform
           end-perform

           move 1 to ind1
           move 1 to ind2
           .
       ganhador-exit.
           exit.


       finaliza section.
           stop run
           .
       finaliza-exit.
           exit.
