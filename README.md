# Escolaridade materna e parto cesáreo no Brasil (SINASC 2024)

Trabalho de Econometria I (FGV EESP)


Autoras: Maria Eduarda Carvalho Sandoval e Maria Carolina Alves de Araujo Tarzia

## Conteúdo

- `TRABALHO.R`: código completo, com limpeza da base, estimações, tabelas e gráficos.
- [Releases](../../releases): contém dois arquivos.
  - `SINASC_2024.csv`: **base original do SINASC 2024, a partir da qual o código começa.** É o arquivo que o script lê.
  - `base_limpa_sinasc2024.rds`: **base já limpa, disponibilizada apenas como opção.** Ela é gerada pelo próprio script (linha 130) e não é necessária para rodá-lo. Serve para quem quiser consultar os dados tratados sem refazer a limpeza. Para abrir no R, use `readRDS("base_final_sinasc2024.rds")`.

A base original também pode ser obtida no portal de dados abertos do Ministério da Saúde: https://dadosabertos.saude.gov.br/dataset/sistema-de-informacao-sobre-nascidos-vivos-sinasc

## Como rodar
1. Baixe o `SINASC_2024.csv` na aba Releases.
2. Abra o `TRABALHO.R` no R e rode o script. Quando aparecer a janela de seleção, escolha o arquivo `SINASC_2024.csv`.
3. As Figuras 5 e 6 são salvas como PNG na pasta de trabalho do R. As demais aparecem no painel de gráficos.

Pacote necessário: `AER`, que já carrega `lmtest` e `sandwich`.

- A base é muito grande. O código remove modelos ao longo da execução para economizar memória.
