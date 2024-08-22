# .Estudos-em-Cobol
Exercícios realizados no curso de Análise e Desenvolvimento de Sistemas, Disciplina de Programação para Mainframe.


## PASSO A PASSO
**Canal "Aprenda Cobol" no youtube:** [link](https://www.youtube.com/watch?v=PHzf0cT8Ia8)
1. Baixar o VSCode - buscar no Google;
2. Instalar o VSCode (seguir as instruçoes);
3. Após instalar, abrir o VSCode e instalar a extensao "IntelliSense - Highlight" apenas;
4. Baixar os binários do GNU COBOL para Windows (compilados) em: [drive](https://drive.google.com/file/d/1vxzDfmOhT21uNzBjasbKg8UjbsJPPGMv/view)
5. Criar uma pasta na unidade raiz (geralmente, c:\) - Exemplo: C:\GNUCOBOL;
6. Fazer as configuraçoes das variaveis de ambiente em "Variaveis do Sistema":
    6.1. Criar a variavel COBOL_HOME e apontar para o diretorio principal do GNU COBOL;
    6.2. Criar as variaveis:
        6.2.1. COB_BIN_DIR:  apontar para sub-diretorio "bin" do GNU COBOL;
        6.2.2. COB_CONFIG_DIR : apontar para sub-diretorio "config" do GNU COBOL;
        6.2.3. COB_COPY_DIR: apontar para sub-diretorio "copy" do GNU COBOL;
        6.2.4. COB_INCLUDE_PATH: apontar para sub-diretorio "lib" do GNU COBOL;
7. Adicionar as variaveis de ambiente de sistema na variável "path" do sistema;
8. Abrir um prompt de comando e testar a chamada com "cobc -v";
9. Se der tudo certo, criar uma workspace e um diretório no VS Code e criar um 
   arquivo .cob nele;
10. Digitar um código COBOL básico (só pra testar);
11. Abrir um novo terminal no VS Code, acessar o diretório e compilar o código criado.

## EXECUTANDO OS ARQUIVOS
**Comando para compilar o arquivo**
- cobc -x PrimeiroPrograma.cob
**Comando para executar o arquivo**
- ./PrimeiroPrograma

