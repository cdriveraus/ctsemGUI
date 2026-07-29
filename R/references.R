# Reference-section content ---------------------------------------------------

ctgui_reference <- function(title, description, url, legacy_code = FALSE) {
  list(
    title = title,
    description = description,
    url = url,
    legacy_code = legacy_code
  )
}

ctgui_reference_catalog <- function() {
  list(
    "Start here" = list(
      ctgui_reference(
        "ctsem GitHub quick start",
        "Current github package landing page with installation, API guidance, minimal workflows, and links to the main resources.",
        "https://github.com/cdriveraus/ctsem"
      ),
      ctgui_reference(
        "Current ctsem manual",
        "Shows data layout, model matrices, fitting, interpretation, hierarchical effects, stationarity, and advanced specifications.",
        "https://cran.r-project.org/package=ctsem/vignettes/hierarchicalmanual.pdf"
      ),
      ctgui_reference(
        "Extensive ctsem tutorial",
        "Long-form tutorial for learning continuous-time concepts alongside ctsem code and examples.",
        "https://osf.io/preprints/psyarxiv/4q9ex_v4"
      ),
      ctgui_reference(
        "ctsem Discussions",
        "Searchable questions, answers, and problem-specific guidance.",
        "https://github.com/cdriveraus/ctsem/discussions"
      )
    ),
    "Core papers" = list(
      ctgui_reference(
        "Driver, C. C., & Voelkle, M. C. (2018). Hierarchical Bayesian continuous time dynamic modeling. Psychological Methods, 23(4), 774–799.",
        "Conceptual paper for hierarchical ctsem: individual differences in dynamic parameters, irregular timing, and Bayesian estimation. Estimation has since moved back to default to optimization approaches with nonlinear filtering, but major concepts here are still relevant.",
        "https://www.researchgate.net/profile/Charles-Driver/publication/324093594_Hierarchical_Bayesian_Continuous_Time_Dynamic_Modeling/links/5c4a029a92851c22a38d6c91/Hierarchical-Bayesian-Continuous-Time-Dynamic-Modeling.pdf"
      ),
      ctgui_reference(
        "Driver, C. C. (2025). Inference with cross-lagged effects—Problems in time. Psychological Methods, 30(1), 174–202.",
        "Conceptual reading on why discrete-time cross-lagged paths can misrepresent underlying processes, and on continuous-time interpretations, regularization, measurement error, model order, and misspecification.",
        "https://osf.io/download/xdf72"
      ),
      ctgui_reference(
        "Driver, C. C., & Voelkle, M. C. (2018). Understanding the time course of interventions with continuous time dynamic models. In K. van Montfort, J. H. L. Oud, & M. C. Voelkle (Eds.), Continuous time modeling in the behavioral and related sciences (pp. 79–109). Springer.",
        "Reading on intervention trajectories modelling: onset, duration, shape, and heterogeneity of effects over time. Provided code needs minor adaptations to modern ctsem, expanded state concepts can be applied to e.g. trend modelling or other structures.",
        "https://www.researchgate.net/profile/Charles-Driver/publication/328221807_Understanding_the_Time_Course_of_Interventions_with_Continuous_Time_Dynamic_Models/links/5f59dc35299bf1d43cf91ce1/Understanding-the-Time-Course-of-Interventions-with-Continuous-Time-Dynamic-Models.pdf"
      ),
      ctgui_reference(
        "Voelkle, M. C., Oud, J. H. L., Davidov, E., & Schmidt, P. (2012). An SEM approach to continuous time modeling of panel data: Relating authoritarianism and anomia. Psychological Methods, 17(2), 176–192.",
        "Foundation for relating discrete-time panel models to an underlying continuous-time process, with an applied example.",
        "https://doi.org/10.1037/a0027543"
      ),
      ctgui_reference(
        "Voelkle, M. C., & Oud, J. H. L. (2013). Continuous time modelling with individually varying time intervals for oscillating and non-oscillating processes. British Journal of Mathematical and Statistical Psychology, 66(1), 103–126.",
        "Why exact and unequal intervals within and between people should be represented in a continuous-time model.",
        "https://doi.org/10.1111/j.2044-8317.2012.02043.x"
      ),
      ctgui_reference(
        "Driver, C. C., Oud, J. H. L., & Voelkle, M. C. (2017). Continuous time structural equation modeling with R package ctsem. Journal of Statistical Software, 77(5), 1–35.",
        "Original ctsem software paper: the model framework and initial R implementation. Provided code and specifics are now legacy (see ctsemOMX package) but much is conceptually still relevant.",
        "https://doi.org/10.18637/jss.v077.i05"
      )
    ),
    "Further reading" = list(
      ctgui_reference(
        "Voelkle, M. C., Gische, C., Driver, C. C., & Lindenberger, U. (2018). The role of time in the quest for understanding psychological mechanisms. Multivariate Behavioral Research, 53(6), 782–805.",
        "Broad conceptual treatment of time, within- and between-person variation, and causal interpretation in longitudinal research.",
        "https://doi.org/10.1080/00273171.2018.1496813"
      ),
      ctgui_reference(
        "Boker, S. M. (2002). Consequences of continuity: The hunt for intrinsic properties within parameters of dynamics in psychological processes. Multivariate Behavioral Research, 37(3), 405–422.",
        "Conceptual argument for considering intrinsic process properties rather than interval-specific descriptions.",
        "https://doi.org/10.1207/S15327906MBR3703_5"
      ),
      ctgui_reference(
        "Aalen, O. O., Røysland, K., Gran, J. M., Kouyos, R., & Lange, T. (2016). Can we believe the DAGs? A comment on the relationship between causal DAGs and mechanisms. Statistical Methods in Medical Research, 25(5), 2294–2314.",
        "Explains why DAGs based on discrete observations need not recover the immediate continuous causal mechanism.",
        "https://doi.org/10.1177/0962280213520436"
      ),
      ctgui_reference(
        "Aalen, O. O. (2018). Feedback and mediation in causal inference illustrated by stochastic process models. Scandinavian Journal of Statistics, 45(1), 62–86.",
        "Discussion of feedback, time-dependent confounding, mediation, and continuous-time causality.",
        "https://doi.org/10.1111/sjos.12286"
      ),
      ctgui_reference(
        "Deboeck, P. R., & Preacher, K. J. (2016). No need to be discrete: A method for continuous time mediation analysis. Structural Equation Modeling, 23(1), 61–75.",
        "Shows why mediation effects in conventional longitudinal models depend on the observation lag, and develops a continuous-time alternative.",
        "https://doi.org/10.1080/10705511.2014.973960"
      ),
      ctgui_reference(
        "Voelkle, M. C., & Oud, J. H. L. (2015). Relating latent change score and continuous time models. Structural Equation Modeling, 22(3), 366–381.",
        "Connects latent change-score and continuous-time models, clarifying their different assumptions about change between observations.",
        "https://doi.org/10.1080/10705511.2014.935918"
      ),
      ctgui_reference(
        "Hamaker, E. L., Kuiper, R. M., & Grasman, R. P. P. P. (2015). A critique of the cross-lagged panel model. Psychological Methods, 20(1), 102–116.",
        "Background on separating stable between-person differences from within-person dynamics in cross-lagged research.",
        "https://doi.org/10.1037/a0038889"
      ),
      ctgui_reference(
        "Ryan, O., & Hamaker, E. L. (2022). Time to intervene: A continuous-time approach to network analysis and centrality. Psychometrika, 87(1), 214–252.",
        "Continuous-time intervention, centrality, and direct/indirect-effect concepts for dynamic networks, with explicit assumptions for causal interpretation.",
        "https://doi.org/10.1007/s11336-021-09767-0"
      )
    ),
    "Blog posts" = list(
      ctgui_reference(
        "Dynamic Systems Modeling in Psychology",
        "QUick conceptual introduction to dynamics, time scales, prediction, and why static snapshots can be insufficient.",
        "https://cdriver.netlify.app/post/introtodynamics/"
      ),
      ctgui_reference(
        "Representations of Dynamic Systems — Problems of SEM / Regression / Discrete Time",
        "Explains how continuously interacting processes induce interval-dependent discrete-time paths, and how to interpret direct effects.",
        "https://cdriver.netlify.app/post/discretetime/"
      ),
      ctgui_reference(
        "Latent Growth Curves, State Dependent Error",
        "Connects growth models to ctsem, then extends them to multivariate and state-dependent measurement-error models.",
        "https://cdriver.netlify.app/post/lgc/",
        legacy_code = TRUE
      ),
      ctgui_reference(
        "Accelerated Longitudinal and Multiple Group Designs in ctsem",
        "Worked guidance for cohort-sequential designs, group moderation, initial-state timing, and model comparison.",
        "https://cdriver.netlify.app/post/accelerated/",
        legacy_code = TRUE
      ),
      ctgui_reference(
        "Discrete Time Models Using ctsem?",
        "Bivariate discrete-time workflow with measurement models, individual differences, prediction, posterior predictive checks, and residual diagnostics.",
        "https://cdriver.netlify.app/post/dtbivariate/",
        legacy_code = TRUE
      )
    )
  )
}

ctgui_reference_card <- function(reference) {
  shiny::tags$article(
    class = "ctgui-reference-item",
    shiny::tags$h4(shiny::tags$a(reference$title, href = reference$url, target = "_blank", rel = "noopener noreferrer")),
    shiny::tags$p(reference$description),
    if (isTRUE(reference$legacy_code)) shiny::tags$p(class = "ctgui-reference-note", "Worked example; its code may use some legacy ctsem function names.")
  )
}

ctgui_reference_ui <- function(catalog = ctgui_reference_catalog()) {
  sections <- lapply(names(catalog), function(title) {
    references <- catalog[[title]]
    shiny::tagList(
      shiny::tags$h4(title),
      shiny::tags$div(class = "ctgui-reference-grid", lapply(references, ctgui_reference_card))
    )
  })

  shiny::div(
    class = "ctgui-reference",
    shiny::tags$h3("ctsem and continuous-time modelling resources"),
    shiny::tags$p(class = "help-note", "Links open in a new tab. Start with the current package resources; papers and posts provide conceptual background and worked examples."),
    sections
  )
}
