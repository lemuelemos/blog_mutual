# blog_mutual

Blog em Quarto para projetos pessoais, posts sobre análise de dados e estudos com R.

## Ambiente

O projeto usa:

- Quarto para renderização do site
- R com `renv` para gerenciamento de dependências
- DuckDB como base local para os estudos

Para restaurar o ambiente:

```r
renv::restore()
```

Para renderizar o site:

```bash
quarto render
```

## Estratégia de artefatos

- `docs/` é o artefato publicado do site e permanece versionado.
- `_freeze/` permanece versionado para preservar cache de execução e evitar reruns pesados em posts antigos.
- `_site/` é tratado como saída local descartável e não deve ser versionado.
- Arquivos transitórios do DuckDB, como `*.duckdb.wal`, não devem ser versionados.

## Bootstrap dos scripts

Os scripts auxiliares em `posts/**/script_*.R` usam `R/bootstrap.R` para:

- localizar a raiz do projeto independentemente do diretório atual
- resolver o caminho do banco DuckDB de forma consistente
- evitar caminhos absolutos ou acoplamento ao diretório de execução
