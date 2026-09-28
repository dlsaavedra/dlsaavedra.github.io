---
layout: page
title: Species Sampling Problem and the EPPF
card_title: Species Sampling and the EPPF
description: Learning a shared law of diversity from distinct collections through their partition sizes.
img: assets/img/projects/species-sampling-eppf.png
importance: 3
---

<figure class="text-center"><img src="{{ '/assets/img/projects/species-sampling-eppf.png' | relative_url }}" alt="Three collections with different category identities but similar cluster-size patterns" class="img-fluid rounded" style="width: 100%; max-width: 520px;"></figure>

A species sampling problem asks how observations are distributed among recurring categories and how many previously unseen categories may appear in future samples. A *species* can be a biological taxon, a word type, a cell clone, or any other repeated entity. For collection \\(j=1,\ldots,J\\), a random discrete distribution gives one representation:

$$
G_j=\sum_{h\geq 1}P_{jh}\,\delta_{\phi_{jh}},
\qquad
X_{ji}\mid G_j\overset{\mathrm{iid}}{\sim}G_j,
\qquad
\phi_{jh}\overset{\mathrm{iid}}{\sim}G_{0j}.
$$

Assume the base distributions \\(G_{0j}\\) have no atoms and the weight vectors follow a common family indexed by \\((\alpha,d)\\). Repeated values then induce a partition \\(\Pi_{j,n_j}\\) of the \\(n_j\\) observations into \\(K_j\\) clusters. If their sizes are \\(n_{j1},\ldots,n_{jK_j}\\), the **exchangeable partition probability function (EPPF)** assigns the probability of a particular partition using only those sizes:

$$
p_{\alpha,d}(n_{j1},\ldots,n_{jK_j})
=\Pr\!\left(\Pi_{j,n_j}=\{A_{j1},\ldots,A_{jK_j}\}\mid\alpha,d\right),
\qquad n_{jh}=|A_{jh}|.
$$

The proposed direction is to study and estimate the behavior of a family of EPPFs indexed by shared diversity and concentration parameters \\((\alpha,d)\\). The EPPF also answers predictive questions. For example, the probability that the next observation forms a new cluster is

$$
\Pr(\text{new cluster}\mid n_{j1},\ldots,n_{jK_j},\alpha,d)
=\frac{p_{\alpha,d}(n_{j1},\ldots,n_{jK_j},1)}
{p_{\alpha,d}(n_{j1},\ldots,n_{jK_j})}.
$$

This framework is especially useful when collections have **different category identities but share a common pattern of diversity and concentration**. The sizes of the clusters matter more than their labels. The base distributions \\(G_{0j}\\) may describe different possible categories in each collection, while \\((\alpha,d)\\) governs a common partition structure.

<div class="table-responsive" markdown="1">

| Application | Collection \\(j\\) | Observation \\(X_{ji}\\) | Cluster | Role of \\(G_{0j}\\) | Shared \\((\alpha,d)\\) |
| --- | --- | --- | --- | --- | --- |
| Ecology | Geographic site | Captured individual | Species | Possible species at the site | Biodiversity pattern |
| Language | Document, author, or language | Word | Word type | Local vocabulary | Lexical richness |
| Immunology | Patient | Immune sequence | Clonotype | Potential repertoire | Clonal diversity |
| Tumor genomics | Patient or tumor | Cell or mutation | Cell clone | Possible tumor mutations | Tumor heterogeneity |
| Consumption | Customer or store | Purchase | Product or category | Available preferences | Purchase concentration |
| Web navigation | User or session | Visit | Page or action type | Accessible content | Behavioral diversity |
| Bibliometrics | Researcher or journal | Citation | Cited article | Relevant literature | Citation concentration |
| Microbiome | Patient or environment | Genetic read | Microbial taxon | Possible microorganisms | Abundance structure |

</div>

For mathematical background on exchangeable partitions and EPPFs, see [Pitman's *Combinatorial Stochastic Processes*](https://www.stat.berkeley.edu/~aldous/206-Exch/Papers/pitman_CSP.pdf).
