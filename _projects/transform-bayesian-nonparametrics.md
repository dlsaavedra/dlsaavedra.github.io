---
layout: page
title: Transform Bayesian Nonparametrics for Heavy-Tailed Distributions
description: A learned transformation makes heavy-tailed data easier to model with simple Gaussian mixtures.
img: assets/img/projects/transform-bnp-heavy-tails.png
importance: 1
---

<figure class="text-center"><img src="{{ '/assets/img/projects/transform-bnp-heavy-tails.png' | relative_url }}" alt="Heavy-tailed observations mapped to a smoother latent distribution and back" class="img-fluid rounded" style="width: 100%; max-width: 520px;"></figure>

Heavy-tailed observations are often difficult to describe with conventional light-tailed kernels on their original scale. This project investigates a **data-adaptive, monotone, invertible transformation** that moves observations into a more regular latent space. In that space, a Dirichlet process (DP) mixture with simple normal kernels can describe the distribution without requiring a complicated kernel for every extreme observation.

Let \(T_\eta\) be the transformation learned from the data. The working model is

$$
z_i=T_\eta(x_i), \qquad
f_Z(z)=\int \phi(z;\mu,\sigma^2)\,G(d\mu,d\sigma^2),
\qquad G\sim\operatorname{DP}(a,G_0).
$$

Inference and prediction take place in the transformed space. The inverse map returns simulated values to the data's original domain, and the change-of-variables formula recovers its density:

$$
x^{\mathrm{new}}=T_\eta^{-1}(z^{\mathrm{new}}), \qquad
f_X(x)=f_Z\!\left(T_\eta(x)\right)\left|T_\eta'(x)\right|.
$$

The central challenge is to learn a transformation that simplifies estimation while preserving the behavior of the original tail after inversion.
