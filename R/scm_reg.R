
#' Use to perform Stepwise Covariate Modeling (SCM)
#'
#' @param dataset name of dataset
#' @param variable dependent variable
#' @param variable_event event variable for coxph regression only
#' @param weights_ordered weight used in the ordered-categorical regression
#' @param covariate.list vector of covariate to be tested
#' @param test_used  test to be applied. Can be 'Chisq' or 'AIC'
#' @param p_forward p value for forward selection and interaction phase
#' @param p_backward p value for forward step
#' @param regression type of regression, can be 'lm', 'logistic', 'coxph' or 'ordered-categorical'
#' @param cov_corr list of correlated covariates you don t want to be included simultaneously like eGFR and CRCL. Can be added as a list of vectors in case of multiple vectors of correlated covariates
#' @param search_direction step to be performed. Can be 'full', 'forward', 'interaction', 'backward', 'forward-backward', 'forward-interaction' or 'interaction-backward'
#' @param base_relation base relation included in the base model. Note those relationships cannot be removed in the backward step. Example "age*bwt + crcl"
#' @param full_relation starting model when performing backward step only.
#' @param max_steps maximal number of covariates included in the forward step
#' @param ... Additional arguments
#'
#' @return
#'
#' @examples
#'  \dontrun{
#'  library(scmreg)
#'  library(MASS)
#'  haha <- scm_reg(dataset=housing,
#'                 variable='Sat',
#'                 covariate.list = c('Infl','Type','Cont'),
#'                 p_forward=0.01,
#'                 p_backward=0.001,
#'                 test_used = 'AIC',
#'                 regression='ordered-categorical',
#'                 search_direction='forward-backward',
#'                 weights_ordered='Freq',
#'                 max_steps=Inf)
#'
#'  }
#' @export

scm_reg <- function(dataset,variable,variable_event=NULL,weights_ordered=NULL,covariate.list=NULL,
                    test_used='Chisq',
                    p_forward=0.01,
                    p_backward=0.001,
                    regression='lm',
                    cov_corr=NULL,
                    search_direction='full',
                    base_relation=NULL,
                    full_relation=NULL,
                    max_steps=Inf,
                    ...){

  if (search_direction=='forward'){
    FORW=TRUE
    INTER=FALSE
    BACK=FALSE
  } else if (search_direction=='forward-interaction'){
    FORW=TRUE
    INTER=TRUE
    BACK=FALSE
  } else if (search_direction=='backward'){
    FORW=FALSE
    INTER=FALSE
    BACK=TRUE
  } else if (search_direction=='full'){
    FORW=TRUE
    INTER=TRUE
    BACK=TRUE
  } else if (search_direction=='interaction'){
    FORW=FALSE
    INTER=TRUE
    BACK=FALSE
  }else if (search_direction=='interaction-backward'){
    FORW=FALSE
    INTER=TRUE
    BACK=TRUE
  }else if (search_direction=='forward-backward'){
    FORW=TRUE
    INTER=FALSE
    BACK=TRUE
  }

  if (!test_used %in% c('AIC','Chisq'))  stop('Only Chisq or AIC tests can be used.')

  if (is.null(covariate.list) & FORW==TRUE )  stop('A covariate.list argunment is needed')

  if (!is.null(full_relation) & (FORW==TRUE | INTER==TRUE))  stop('full_relation cannot be used with forward and/or interaction phases')

  if (regression=='coxph' & is.null(variable_event))  stop('argument variable_event necessary with coxph regression')

  if (regression=='ordered-categorical' & INTER==T)  stop('Interaction option not tested with ordered categorical regression')

  #if (test_used=='AIC')  stop('AIC cannot be used, no possibility to compute p value. If you want to use AIC please switch to the "step" function')

  if (max_steps<1)  stop('max_steps should be at least equal to 1')

  if (!is.null(base_relation) & any((stringr::str_trim(unlist(unique(stringr::str_split(gsub('\\*', "\\+",base_relation), "\\+"))))) %in% covariate.list)){
    stop("covariates in the base_relation are also included in the covariates list, please remove them from the covariates list")

  }

  if (test_used=='AIC')  print('While AIC is selected, p-values will not be used')

  forw_inter_possible <- FALSE



  tracelog=TRUE

  tibbleList <- c('term','df','rss','AIC','p.value','Step','direction','select')#,'Selected','To keep')

  df <- matrix(ncol = length(tibbleList), nrow = 0)
  colnames(df) <- tibbleList
  tabtot <-as.data.frame(df)

  col_red=crayon::red
  col_green=crayon::green
  col_blue=crayon::blue

  coco_select_back <- reg.full.int <- reg.full.forw <- NULL

  forward_int_cov <- forward_cov <- NULL

  cov_base2 <- cov_base <- NULL

  coco_select_forw_tot <- coco_select_forw <- NULL

  ######## Forward

  if (FORW==TRUE) {

    #cov_back_only <- NULL # to overcome possible missuse of the cov_back_only argument with FORW=TRUE argument


    coco <- glue::glue_collapse(covariate.list , sep = "+")

    coco_select_forw <- NULL

    if (is.null(base_relation) & regression=='lm') {

      mod0=glm(as.formula(glue::glue('{variable} ~ 1')),data=dataset)

    }else if (!is.null(base_relation) & regression=='lm') {
      #base_relation <- 'lsoda*coca +orangina'

      mod0=glm(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset)

      # in order to get all parameters that are ot allowed to be removed
      cov_base <- gsub('\\*', "\\+",base_relation)
      cov_base <- stringr::str_trim(unlist(unique(stringr::str_split(cov_base, "\\+"))))

      cov_base_tmp <- broom::tidy(mod0)%>%
        dplyr::filter(term!='(Intercept)')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base_tmp, ":")
      reversed <- purr::map(splits, rev)
      cov_base_tmp2 <- purr::map_chr(reversed , paste, collapse = ":")
      cov_base <- c(cov_base_tmp2,cov_base_tmp,cov_base)
      cov_base <-unique(cov_base)

    }else if (is.null(base_relation) & regression=='logistic') {

      mod0=glm(as.formula(glue::glue('{variable} ~ 1')),data=dataset,family='binomial')

    }else if (!is.null(base_relation) & regression=='logistic') {
      #
      mod0=glm(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset,family='binomial')

      # in order to get all parameters that are ot allowed to be removed
      cov_base <- gsub('\\*', "\\+",base_relation)
      cov_base <- unlist(unique(stringr::str_split(cov_base, "\\+")))

      cov_base_tmp <- broom::tidy(mod0)%>%
        dplyr::filter(term!='(Intercept)')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()


      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base_tmp, ":")
      reversed <- purrr::map(splits, rev)
      cov_base_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
      cov_base <- c(cov_base_tmp2,cov_base_tmp,cov_base)
      cov_base <-unique(cov_base)

    }else if (is.null(base_relation) & regression=='coxph') {

      mod0=survival::coxph(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~1')),data=dataset)

    }else if (!is.null(base_relation) & regression=='coxph') {
      #base_relation <- 'lsoda*coca +orangina'

      mod0=survival::coxph(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{base_relation}')),data=dataset)

      # in order to get all parameters that are ot allowed to be removed
      cov_base <- gsub('\\*', "\\+",base_relation)
      cov_base <- unlist(unique(stringr::str_split(cov_base, "\\+")))

      cov_base_tmp <- broom::tidy(mod0)%>%
        dplyr::filter(term!='(Intercept)')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base_tmp, ":")
      reversed <- purrr::map(splits, rev)
      cov_base_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
      cov_base <- c(cov_base_tmp2,cov_base_tmp,cov_base)
      cov_base <-unique(cov_base)

    }else if (is.null(base_relation) & regression=='ordered-categorical') {

      if (!is.null(weights_ordered)){
        mod0=MASS::polr(as.formula(glue::glue('{variable} ~ 1')),data=dataset, Hess = T,weights=eval(sym(weights_ordered))) ## Hess=T added to avoid error message, see https://stackoverflow.com/questions/39151409/potential-bug-in-rs-polr-function-when-run-from-a-function-environment

      } else if (is.null(weights_ordered)){

        mod0=MASS::polr(as.formula(glue::glue('{variable} ~ 1')),data=dataset, Hess = T)
      }

    }else if (!is.null(base_relation) & regression=='ordered-categorical') {

      if (!is.null(weights_ordered)){
        mod0=MASS::polr(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset, Hess = T,weights=eval(sym(weights_ordered)))

      } else if (is.null(weights_ordered)){
        mod0=MASS::polr(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset, Hess = T)

      }
      # in order to get all parameters that are ot allowed to be removed
      cov_base <- gsub('\\*', "\\+",base_relation)
      cov_base <- unlist(unique(stringr::str_split(cov_base, "\\+")))

      cov_base_tmp <- broom::tidy(mod0)%>%
        dplyr::filter(coef.type!='scale')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base_tmp, ":")
      reversed <- purrr::map(splits, rev)
      cov_base_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
      cov_base <- c(cov_base_tmp2,cov_base_tmp,cov_base)
      cov_base <-unique(cov_base)

    }


    for (i in 1:length(covariate.list)) {



      #i=3
      if (i==1 & is.null(base_relation)) {
        tutu <-  add1(mod0,scope = as.formula(glue::glue('~ {coco}')),data=dataset,trace=tracelog,test='Chisq')

      } else if (i==1 & !is.null(base_relation)) {
        tutu <-  add1(mod0,scope = as.formula(glue::glue('~ . +{coco}')),data=dataset,trace=tracelog,test='Chisq')


      } else if (i>1 & is.null(base_relation)){

        coco_select_forw_reg <- glue::glue_collapse(coco_select_forw , sep = "+")

        tutu <-  add1(update(mod0, as.formula(glue::glue(' ~ . + {coco_select_forw_reg}'))),scope = as.formula(glue::glue('~ {coco}')),data=dataset,trace=tracelog,test='Chisq')

      } else if (i>1 & !is.null(base_relation)){

        coco_select_forw_reg <- glue::glue_collapse(coco_select_forw , sep = "+")

        tutu <-  add1(update(mod0, as.formula(glue::glue(' ~ . + {coco_select_forw_reg}'))),scope = as.formula(glue::glue('~ .+{coco}')),data=dataset,trace=tracelog,test='Chisq')

      }

      # located here to have   coco_select_forw_reg updated
      if (i>max_steps) {

        print(glue::glue_col('{col_red Forward step completed, {max_steps} max forward steps reached}'))

        print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_forw,sep=", ",last=" and ")} selected }'))

        print(summary(update(mod0, as.formula(glue::glue(' ~ .+ {coco_select_forw_reg}')))))

        reg.full <- update(mod0, as.formula(glue::glue(' ~ .+ {coco_select_forw_reg}')))


        coco_select_forw_tot_string <- as.character(reg.full$terms[[3]])  # to take into account potential base relationship

        coco_select_forw_tot <- unlist(unique(stringr::str_split(coco_select_forw_tot_string, ' ')))
        coco_select_forw_tot <- coco_select_forw_tot[!coco_select_forw_tot %in% '+'] # to remove + and keep only real covariates

        #if (length(coco_select_forw)>=2) {  # 2 covariates needed for interaction
        if (length(coco_select_forw_tot)>=2) {  # 2 covariates needed for interaction

          forw_inter_possible <- TRUE
        } else {

          forw_inter_possible <- FALSE

        }

        print(tabtot)
        break

      }


      print(tutu)




      if (i==1 & test_used=='Chisq' ) {
        coco_select_forw_tmp <- broom::tidy(tutu)%>%
          dplyr::filter(term!='<none>')  %>%
          dplyr::filter(p.value<=p_forward)%>%
          dplyr::arrange(p.value) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()

      } else if (i==1 & test_used=='AIC' ) {

        # get AIC of reference model
        AIC_ref <- broom::tidy(tutu) %>%
          dplyr::filter(term=='<none>') %>% dplyr::pull(AIC)


        coco_select_forw_tmp <- broom::tidy(tutu)%>%
          dplyr::filter(term!='<none>')  %>%
          dplyr::filter(AIC<=AIC_ref)%>%
          dplyr::arrange(AIC) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()



      }else {
        coco_select_forw_tmp_2 <- broom::tidy(tutu)%>%
          dplyr::filter(term!='<none>')

        # get AIC of reference model
        AIC_ref <- broom::tidy(tutu) %>%
          dplyr::filter(term=='<none>') %>% dplyr::pull(AIC)

        if (!is.null(cov_corr)){
          for (t in coco_select_forw) {
            if (any(purrr::map_lgl(cov_corr, `%in%`, x = t))) {
              coco_select_forw_tmp_2 <- dplyr::filter(coco_select_forw_tmp_2, !term %in% unlist(cov_corr[purrr::map_lgl(cov_corr, `%in%`, x =t)]))
            }else {
              coco_select_forw_tmp_2 <-  dplyr::filter(coco_select_forw_tmp_2, !is.na(term))
            }
          }
        }

        if (test_used=='Chisq') {
          coco_select_forw_tmp<- coco_select_forw_tmp_2 %>%
            dplyr::filter(p.value<=p_forward)%>%
            dplyr::arrange(p.value) %>%
            dplyr::slice(1) %>%
            dplyr::pull(term) %>% unique()

        } else if (test_used=='AIC') {

          # get AIC of reference model

          coco_select_forw_tmp<- coco_select_forw_tmp_2 %>%
            dplyr::filter(AIC<=AIC_ref)%>%
            dplyr::arrange(AIC) %>%
            dplyr::slice(1) %>%
            dplyr::pull(term) %>% unique()}
      }

      if (!rlang::is_empty(coco_select_forw_tmp)) {
        tabtot <- rbind(tabtot,broom::tidy(tutu)%>% dplyr::mutate(direction='forward',
                                                                  step=i,
                                                                  select=ifelse(term==coco_select_forw_tmp ,1,0)))
      }

      if (rlang::is_empty(coco_select_forw_tmp)) {

        print(glue::glue_col('{col_red Forward step completed}'))
        tabtot <- rbind(tabtot,broom::tidy(tutu)%>% dplyr::mutate(direction='forward',
                                                                  step=i,
                                                                  select=0))

        if (!is.null(coco_select_forw)) {

          print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_forw,sep=", ",last=" and ")} selected }'))

          print(summary(update(mod0, as.formula(glue::glue(' ~ .+ {coco_select_forw_reg}')))))

          reg.full <- update(mod0, as.formula(glue::glue(' ~ .+ {coco_select_forw_reg}')))



        } else {

          print('No covariate included')
          reg.full <- mod0
        }

        coco_select_forw_tot_string <- as.character(reg.full$terms[[3]])  # to take into account potential base relationship

        coco_select_forw_tot <- unlist(unique(stringr::str_split(coco_select_forw_tot_string, ' ')))

        # remove '1' null model from list
        coco_select_forw_tot <-coco_select_forw_tot[!coco_select_forw_tot %in% '1']

        if (rlang::is_empty(coco_select_forw_tot)) {

          coco_select_forw_tot <- NULL

        } else if (!rlang::is_empty(coco_select_forw_tot)){

          coco_select_forw_tot <- coco_select_forw_tot[!coco_select_forw_tot %in% '+'] # to remove + and keep only real covariates
        }
        #if (length(coco_select_forw)>=2) {  # 2 covariates needed for interaction
        if (length(coco_select_forw_tot)>=2) {  # 2 covariates needed for interaction

          forw_inter_possible <- TRUE
        } else {

          forw_inter_possible <- FALSE

        }

        print(tabtot)
        break

      }

      print(glue::glue('covariate {coco_select_forw_tmp} selected for step {i}'))

      coco_select_forw <- c(coco_select_forw, coco_select_forw_tmp)
      print(coco_select_forw)

      if (i==length(covariate.list)){

        print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_forw,sep=", ",last=" and ")} selected }'))




        coco_select_forw_reg <- glue::glue_collapse(coco_select_forw , sep = "+")

        print(summary(update(mod0, as.formula(glue::glue(' ~ .+ {coco_select_forw_reg}')))))

        reg.full <- update(mod0, as.formula(glue::glue(' ~ .+ {coco_select_forw_reg}')))

        coco_select_forw_tot_string <- as.character(reg.full$terms[[3]])  # to take into account potential base relationship

        coco_select_forw_tot <- unlist(unique(stringr::str_split(coco_select_forw_tot_string, ' ')))
        coco_select_forw_tot <- coco_select_forw_tot[!coco_select_forw_tot %in% '+'] # to remove + and keep only real covariates

        if (length(coco_select_forw_tot)>=2) {  # 2 covariates needed for interaction

          forw_inter_possible <- TRUE
        } else {

          forw_inter_possible <- FALSE

        }
      }


    }

    reg.full.forw <- reg.full
    forward_cov <- coco_select_forw

    if (INTER==FALSE & BACK==FALSE) {
      final_mod <- reg.full
      tryCatch({
        print(broom::tidy(final_mod))
      },
      error = function(e){
        return()
      })
      final_cov <- coco_select_forw
    }


  }

  ######## Interaction part
  if (FORW==FALSE & is.null(base_relation))  forw_inter_possible <- FALSE

  if (INTER==TRUE & !is.null(base_relation))  forw_inter_possible <- TRUE


  if (INTER==TRUE & FORW==FALSE & is.null(base_relation)) {
    print('Interaction step cannot be performed without a forward step and a base model')
  }

  if (forw_inter_possible == FALSE) {
    print(glue::glue_col('{col_red Interaction step cannot be performed, not enough covariates included in the forward step}'))


  }


  if (INTER==TRUE & forw_inter_possible == TRUE & FORW==TRUE) {
    #if (INTER==TRUE & forw_inter_possible == TRUE) {


    # pair by pair interaction
    coco_inter <- coco_select_inter <- coco_select_forw_inter <- NULL


    if (is.null(base_relation)) {

      if (length(coco_select_forw)>2) {  ## does not work when only 2 variables

        coco_select_inter <-  purrr::map_chr(combn(coco_select_forw, 2,simplify=F),paste,collapse='*')

      } else{

        coco_select_inter <- glue::glue_collapse(coco_select_forw , sep = "*")
      }

    } else if (!is.null(base_relation)){

      #cov_base_int <- gsub('\\*', "\\+",base_relation)
      cov_base_int <- stringr::str_trim(unlist(unique(stringr::str_split(base_relation ,"\\+"))))
      cov_base_int <- cov_base_int[stringr::str_detect(cov_base_int,'\\*')]
      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      if (!rlang::is_empty(cov_base_int)){
        splits <- strsplit(cov_base_int, "\\*")
        reversed <-purrr::map(splits, rev)
        cov_base_int_tmp <-purrr::map_chr(reversed , paste, collapse = '*')
        cov_base_int <- c(cov_base_int_tmp,cov_base_int)
        cov_base_int <-unique(cov_base_int)
      }

      coco_select_forw_tot_tmp <- coco_select_forw_tot[!stringr::str_detect(coco_select_forw_tot,':')]

      if (length(coco_select_forw_tot_tmp)>2) {  ## does not work when only 2 variables

        #i<-1
        coco_select_inter <-  purrr::map_chr(combn(coco_select_forw_tot_tmp, 2,simplify=F),paste,collapse='*')


      } else{

        coco_select_inter <- glue::glue_collapse(coco_select_forw_tot_tmp , sep = "*")
      }

      coco_select_inter <- coco_select_inter[!coco_select_inter %in% cov_base_int]
    }

    #
    coco3 <- glue::glue_collapse(coco_select_inter , sep = "+")


    for (i in 1:length(coco_select_inter)) {
      #i=1
      if (i==1) { #& is.null(base_relation)) {
        #tyty <-  add1(reg.full,scope = as.formula(glue('~ {coco3}')),data=dataset,trace=tracelog,test='Chisq')
        tyty <-  add1(reg.full,scope = as.formula(glue::glue('~ . + {coco3}')),data=dataset,trace=tracelog,test='Chisq')

        #} else if (i==1 & !is.null(base_relation)){

        #  tyty <-  add1(reg.full,scope = as.formula(glue('~ .+{coco3}')),data=dataset,trace=tracelog,test='Chisq')

      } else {

        coco_select_forw_inter_reg <- glue::glue_collapse(coco_select_forw_inter , sep = "+")

        tyty <-  add1(update(reg.full, as.formula(glue::glue(' ~ . + {coco_select_forw_inter_reg}'))),scope = as.formula(glue::glue('~ {coco3}')),data=dataset,trace=tracelog,test='Chisq')

      }

      print(tyty)



      if (test_used=='Chisq' ) {
        coco_select_forw_inter_tmp <- broom::tidy(tyty)%>%
          dplyr::filter(term!='<none>')  %>%
          dplyr::filter(p.value<=p_forward)%>%
          dplyr::arrange(p.value) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()

      } else if (test_used=='AIC' ){

        # get AIC of reference model
        AIC_ref <- broom::tidy(tyty) %>%
          dplyr::filter(term=='<none>') %>% dplyr::pull(AIC)

        coco_select_forw_inter_tmp <- broom::tidy(tyty)%>%
          dplyr::filter(term!='<none>')  %>%
          dplyr::filter(AIC<=AIC_ref)%>%
          dplyr::arrange(AIC) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()

      }

      if (!rlang::is_empty(coco_select_forw_inter_tmp)) {
        tabtot <- rbind(tabtot,broom::tidy(tyty)%>%
                          dplyr::mutate(direction='forward interaction',
                                                           step=i,
                                                           select=ifelse(term==coco_select_forw_inter_tmp,1,0)))
      }
      if (rlang::is_empty(coco_select_forw_inter_tmp)) {

        print(glue::glue_col('{col_red Forward interaction step completed}'))

        tabtot <- rbind(tabtot,broom::tidy(tyty)%>%
                          dplyr::mutate(direction='forward interaction',
                                                           step=i,
                                                           select=0))

        if (!is.null(coco_select_forw_inter)) {

          print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_forw_inter,sep=", ",last=" and ")} selected }'))

          print(summary(update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))))

          reg.full <- update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))

        } else {
          print(glue::glue_col('{col_green No interaction term included}'))

        }
        print(tabtot)

        break
      }

      print(glue::glue('covariate {coco_select_forw_inter_tmp} selected for step {i}'))

      coco_select_forw_inter <- c(coco_select_forw_inter, coco_select_forw_inter_tmp)
      print(coco_select_forw_inter)

      if (i==length(coco_select_inter)){

        print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_forw_inter,sep=", ",last=" and ")} selected }'))


        coco_select_forw_inter_reg <- glue::glue_collapse(coco_select_forw_inter , sep = "+")

        print(summary(update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))))

        reg.full <- update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))


      }

    }


    if (!is.null(coco_select_forw_inter)) {

      coco_select_forw <- c(coco_select_forw ,coco_select_forw_inter)

    }

    reg.full.int <- reg.full
    forward_int_cov <- coco_select_forw[!coco_select_forw %in% forward_cov]

    if (!rlang::is_empty(forward_int_cov)) forward_int_cov <- NULL

    if (BACK==FALSE) {
      final_mod <- reg.full
      tryCatch({
        print(broom::tidy(final_mod))
      },
      error = function(e){
        return()
      })
      final_cov <- coco_select_forw
    }

    # test to be made
    coco_select_forw_tot_string <- as.character(reg.full$terms[[3]])  # to take into account potential base relationship

    coco_select_forw_tot <- unlist(unique(stringr::str_split(coco_select_forw_tot_string, ' ')))
    coco_select_forw_tot <- coco_select_forw_tot[!coco_select_forw_tot %in% '+'] # to remove + and keep only real covariates

  }

  ############################## to be continued here

  if (INTER==TRUE & forw_inter_possible == TRUE & FORW==FALSE) {

    coco_select_forw <- base_relation
    # pair by pair interaction
    coco_inter <- coco_select_inter <- coco_select_forw_inter <- NULL




    if (regression=='lm') { reg.full <- glm(as.formula(glue::glue('{variable} ~{base_relation}')),data=dataset)

    }

    if (regression=='logistic') { reg.full <- glm(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset,family='binomial')

    }

    if (regression=='coxph') {reg.full <- survival::coxph(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{base_relation}')),data=dataset)

    }

    if (regression=='ordered-categorical') {

      if (!is.null(weights_ordered)){

        reg.full <- MASS::polr(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset, Hess = T,weights=eval(sym(weights_ordered)))

      } else if (is.null(weights_ordered)){

        reg.full <- MASS::polr(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset, Hess = T)

      }
    }


    cov_base_int <- stringr::str_trim(unlist(unique(stringr::str_split(base_relation ,"\\+"))))

    if(any(stringr::str_detect(cov_base_int,'\\*'))) stop('Interaction term already included, please remove it from the base relation')

    if(length(cov_base_int)<2) stop('Not enough covariate in the base relation')

    coco_select_forw_tot_tmp <- cov_base_int

    if (length(coco_select_forw_tot_tmp)>2) {  ## does not work when only 2 variables

      #i<-1
      coco_select_inter <-  purrr::map_chr(combn(coco_select_forw_tot_tmp, 2,simplify=F),paste,collapse='*')


    } else{

      coco_select_inter <- glue::glue_collapse(coco_select_forw_tot_tmp , sep = "*")
    }

    coco_select_inter <- coco_select_inter[!coco_select_inter %in% cov_base_int]

    #
    coco3 <- glue::glue_collapse(coco_select_inter , sep = "+")


    for (i in 1:length(coco_select_inter)) {
      #i=1
      if (i==1) { #& is.null(base_relation)) {
        #tyty <-  add1(reg.full,scope = as.formula(glue::glue('~ {coco3}')),data=dataset,trace=tracelog,test='Chisq')
        tyty <-  add1(reg.full,scope = as.formula(glue::glue('~ . + {coco3}')),data=dataset,trace=tracelog,test='Chisq')


      } else {

        coco_select_forw_inter_reg <- glue::glue_collapse(coco_select_forw_inter , sep = "+")

        tyty <-  add1(update(reg.full, as.formula(glue::glue(' ~ . + {coco_select_forw_inter_reg}'))),scope = as.formula(glue::glue('~ {coco3}')),data=dataset,trace=tracelog,test='Chisq')

      }

      print(tyty)



      if (test_used=='Chisq' ) {
        coco_select_forw_inter_tmp <- broom::tidy(tyty)%>%
          dplyr::filter(term!='<none>')  %>%
          dplyr::filter(p.value<=p_forward)%>%
          dplyr::arrange(p.value) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()

      } else if(test_used=='AIC' ) {


        # get AIC of reference model
        AIC_ref <- broom::tidy(tyty) %>%
          dplyr::filter(term=='<none>') %>% dplyr::pull(AIC)

        coco_select_forw_inter_tmp <- broom::tidy(tyty)%>%
          dplyr::filter(term!='<none>')  %>%
          dplyr::filter(AIC<=AIC_ref)%>%
          dplyr::arrange(AIC) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()

      }



      if (!rlang::is_empty(coco_select_forw_inter_tmp)) {
        tabtot <- rbind(tabtot,broom::tidy(tyty)%>% dplyr::mutate(direction='forward interaction',
                                                           step=i,
                                                           select=ifelse(term==coco_select_forw_inter_tmp,1,0)))
      }
      if (rlang::is_empty(coco_select_forw_inter_tmp)) {

        print(glue::glue_col('{col_red Forward interaction step completed}'))

        tabtot <- rbind(tabtot,broom::tidy(tyty)%>% dplyr::mutate(direction='forward interaction',
                                                           step=i,
                                                           select=0))

        if (!is.null(coco_select_forw_inter)) {

          print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_forw_inter,sep=", ",last=" and ")} selected }'))

          print(summary(update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))))

          reg.full <- update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))

        } else {
          print(glue::glue_col('{col_green No interaction term included}'))

        }
        print(tabtot)

        break
      }

      print(glue::glue('covariate {coco_select_forw_inter_tmp} selected for step {i}'))

      coco_select_forw_inter <- c(coco_select_forw_inter, coco_select_forw_inter_tmp)
      print(coco_select_forw_inter)

      if (i==length(coco_select_inter)){

        print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_forw_inter,sep=", ",last=" and ")} selected }'))


        coco_select_forw_inter_reg <- glue::glue_collapse(coco_select_forw_inter , sep = "+")

        print(summary(update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))))

        reg.full <- update(reg.full, as.formula(glue::glue(' ~ .+ {coco_select_forw_inter_reg}')))


      }

    }

    ## to check
    if (!is.null(coco_select_forw_inter)) {

      coco_select_forw <- coco_select_forw_inter

    }

    reg.full.int <- reg.full
    forward_int_cov <- coco_select_forw

    if (!rlang::is_empty(forward_int_cov)) forward_int_cov <- NULL

    if (BACK==FALSE) {
      final_mod <- reg.full
      tryCatch({
        print(broom::tidy(final_mod))
        },
        error = function(e){
          return()
        })
      final_cov <- coco_select_forw
    }

    # test to be made
    coco_select_forw_tot_string <- as.character(reg.full$terms[[3]])  # to take into account potential base relationship

    coco_select_forw_tot <- unlist(unique(stringr::str_split(coco_select_forw_tot_string, ' ')))
    coco_select_forw_tot <- coco_select_forw_tot[!coco_select_forw_tot %in% '+'] # to remove + and keep only real covariates

  }



  ######## Backward
  if (FORW==FALSE & INTER==FALSE & BACK==TRUE ) {

    if (is.null(full_relation)){
      stop("full_relation argument cannot be empty when performing backward step only")

    }

    if (!is.null(base_relation)) {
		if (!grepl(stringr::str_replace_all(stringr::str_squish(base_relation)," ",""), stringr::str_replace_all(stringr::str_squish(full_relation)," ",""), fixed = TRUE)){

	  stop("base_relation not included in full_relation argument (or change the order)")

     }
	}

    cocoback <- full_relation

    if (regression=='lm') { reg.full <- glm(as.formula(glue::glue('{variable} ~ {cocoback}')),data=dataset)

    }

    if (regression=='logistic') { reg.full <- glm(as.formula(glue::glue('{variable} ~ {cocoback}')),data=dataset,family='binomial')

    }

    if (regression=='coxph') {reg.full <- survival::coxph(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{cocoback}')),data=dataset)

    }

    if (regression=='ordered-categorical') {
      if (!is.null(weights_ordered)){
        reg.full <- MASS::polr(as.formula(glue::glue('{variable} ~ {cocoback}')),data=dataset, Hess = T,weights=eval(sym(weights_ordered)))

      }else if (is.null(weights_ordered)){
        reg.full <- MASS::polr(as.formula(glue::glue('{variable} ~ {cocoback}')),data=dataset, Hess = T)


      }

    }

    #} # to check
    if (is.null(base_relation) ) {

      cov_base2 <- NULL

    }else if (!is.null(base_relation) & regression=='lm') {
      #base_relation <- 'lsoda*coca +orangina'

      mod0bis=glm(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset)

      # in order to get all parameters that are not allowed to be removed
      cov_base2 <- gsub('\\*', "\\+",base_relation)
      cov_base2 <- stringr::str_trim(unlist(unique(stringr::str_split(cov_base2, "\\+"))))

      cov_base2_tmp <- broom::tidy(mod0bis)%>%
        dplyr::filter(term!='(Intercept)')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base2_tmp, ":")
      reversed <- purrr::map(splits, rev)
      cov_base2_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
      cov_base2 <- c(cov_base2_tmp2,cov_base2_tmp,cov_base2)
      cov_base2 <-unique(cov_base2)


    }else if (!is.null(base_relation) & regression=='logistic') {
      #
      mod0bis=glm(as.formula(glue::glue('{variable} ~ {base_relation}')),data=dataset,family='binomial')

      # in order to get all parameters that are ot allowed to be removed
      cov_base2 <- gsub('\\*', "\\+",base_relation)
      cov_base2 <- unlist(unique(stringr::str_split(cov_base2, "\\+")))

      cov_base2_tmp <- broom::tidy(mod0bis)%>%
        dplyr::filter(term!='(Intercept)')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base2_tmp, ":")
      reversed <- purrr::map(splits, rev)
      cov_base2_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
      cov_base2 <- c(cov_base2_tmp2,cov_base2_tmp,cov_base2)
      cov_base2 <-unique(cov_base2)


    }else if (!is.null(base_relation) & regression=='coxph') {
      #base_relation <- 'lsoda*coca +orangina'

      mod0bis=survival::coxph(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{base_relation}')),data=dataset)

      # in order to get all parameters that are ot allowed to be removed
      cov_base2 <- gsub('\\*', "\\+",base_relation)
      cov_base2 <- unlist(unique(stringr::str_split(cov_base2, "\\+")))

      cov_base2_tmp <- broom::tidy(mod0bis)%>%
        dplyr::filter(term!='(Intercept)')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base2_tmp, ":")
      reversed <- purrr::map(splits, rev)
      cov_base2_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
      cov_base2 <- c(cov_base2_tmp2,cov_base2_tmp,cov_base2)
      cov_base2 <-unique(cov_base2)

    }else if (!is.null(base_relation) & regression=='ordered-categorical') {
      #base_relation <- 'lsoda*coca +orangina'

      if (!is.null(weights_ordered)){
        mod0bis=MASS::polr(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{base_relation}')),data=dataset, Hess = T,weights=eval(sym(weights_ordered)))

      } else if (is.null(weights_ordered)) {

        mod0bis=MASS::polr(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{base_relation}')),data=dataset, Hess = T)

      }
      # in order to get all parameters that are ot allowed to be removed
      cov_base2 <- gsub('\\*', "\\+",base_relation)
      cov_base2 <- unlist(unique(stringr::str_split(cov_base2, "\\+")))

      cov_base2_tmp <- broom::tidy(mod0bis)%>%
        dplyr::filter(coef.type!='scale')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

      # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
      splits <- strsplit(cov_base2_tmp, ":")
      reversed <- purrr::map(splits, rev)
      cov_base2_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
      cov_base2 <- c(cov_base2_tmp2,cov_base2_tmp,cov_base2)
      cov_base2 <-unique(cov_base2)

    }
    ## Definition of coco_select_forw_tot and coco_select_forw
    # in order to get all parameters that are ot allowed to be removed
    coco_select_forw_tot <- gsub('\\*', "\\+",full_relation)
    coco_select_forw_tot <- stringr::str_trim(unlist(unique(stringr::str_split(coco_select_forw_tot, "\\+"))))

    if (regression != 'ordered-categorical') {
      coco_select_forw_tot_tmp <- broom::tidy(reg.full)%>%
        dplyr::filter(term!='(Intercept)')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()

    } else if (regression == 'ordered-categorical') {

      coco_select_forw_tot_tmp <- broom::tidy(reg.full)%>%
        dplyr::filter(coef.type!='scale')%>%
        dplyr::filter(stringr::str_detect(term,':')) %>%
        dplyr::pull(term) %>% unique()
    }


    # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
    splits <- strsplit(coco_select_forw_tot_tmp, ":")
    reversed <- purrr::map(splits, rev)
    coco_select_forw_tot_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
    coco_select_forw_tot <- c(coco_select_forw_tot_tmp2,coco_select_forw_tot_tmp,coco_select_forw_tot)
    coco_select_forw_tot <-unique(coco_select_forw_tot)
    coco_select_forw <- coco_select_forw_tot

  } #old parenthesis


  if (FORW==FALSE & INTER==TRUE & BACK==TRUE ) {
    # in order to get all parameters that are ot allowed to be removed
    cov_base <- gsub('\\*', "\\+",base_relation)
    cov_base <- unlist(unique(stringr::str_split(cov_base, "\\+")))

    cov_base_tmp <- broom::tidy(mod0)%>%
      dplyr::filter(term!='(Intercept)')%>%
      dplyr::filter(stringr::str_detect(term,':')) %>%
      dplyr::pull(term) %>% unique()

    # sometimes with correlation it is reported as A:B or B:A, so I take both cases in the list to be sure
    splits <- strsplit(cov_base_tmp, ":")
    reversed <- purrr::map(splits, rev)
    cov_base_tmp2 <- purrr::map_chr(reversed , paste, collapse = ":")
    cov_base <- c(cov_base_tmp2,cov_base_tmp,cov_base)
    cov_base <-unique(cov_base)
  }

  #if (!is.null(coco_select_forw) & BACK==TRUE) {
  if (!is.null(coco_select_forw_tot) & BACK==TRUE) {

    coco_select_back <- NULL

    #for (j in 1:(length(coco_select_forw)+1)) {
    for (j in 1:(length(coco_select_forw_tot)+1)) {
      #j=7
      if (j==1) {

        tata <-  drop1(reg.full,data=dataset,trace=tracelog,test='Chisq')

      }  else {

        coco_select_back_reg <- glue::glue_collapse(coco_select_back , sep = "-")

        tata <-  drop1(update(reg.full, as.formula(glue::glue(' ~ .-{coco_select_back_reg}'))),data=dataset,trace=tracelog,test='Chisq')


      }

      print(tata)


      if (test_used=='Chisq' ) {
        coco_select_back_tmp <- broom::tidy(tata)%>%
          dplyr::filter(term!='<none>')  %>%
          ##filter(term!='variable_to_keep_if_needed')  %>%
          {if (!is.null(cov_base)) dplyr::filter(., !term %in% cov_base) else dplyr::filter(., !is.na(term))} %>%
          {if (!is.null(cov_base2)) dplyr::filter(., !term %in% cov_base2) else dplyr::filter(., !is.na(term))} %>%
          dplyr::filter(p.value>p_backward)%>%
          dplyr::arrange(-p.value) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()

      } else if (test_used=='AIC' ) {

        # get AIC of reference model
        AIC_ref <- broom::tidy(tata) %>%
          dplyr::filter(term=='<none>') %>% dplyr::pull(AIC)


        coco_select_back_tmp <- broom::tidy(tata)%>%
          dplyr::filter(term!='<none>')  %>%

          {if (!is.null(cov_base)) dplyr::filter(., !term %in% cov_base) else dplyr::filter(., !is.na(term))} %>%
          {if (!is.null(cov_base2)) dplyr::filter(., !term %in% cov_base2) else dplyr::filter(., !is.na(term))} %>%
          dplyr::filter(AIC<=AIC_ref)%>%
          dplyr::arrange(AIC) %>%
          dplyr::slice(1) %>%
          dplyr::pull(term) %>% unique()

      }

      print(coco_select_back_tmp)

      if (!rlang::is_empty(coco_select_back_tmp)) {
        tabtot <- rbind(tabtot,broom::tidy(tata)%>% dplyr::mutate(direction='backward',
                                                           step=j,
                                                           select=ifelse(term==coco_select_back_tmp,1,0)))

      }

      if (rlang::is_empty(coco_select_back_tmp)) {

        tabtot <- rbind(tabtot,broom::tidy(tata)%>% dplyr::mutate(direction='backward',
                                                           step=j,
                                                           select=0))
        if (!is.null(coco_select_back)) {

          splits <- strsplit(coco_select_back, ":")
          reversed <- purrr::map(splits, rev)
          coco_select_back_fin <- purrr::map_chr(reversed , paste, collapse = ":")

          coco_select_back_fin <- c(coco_select_back_fin,coco_select_back)

          coco_select_back_fin <-unique(coco_select_back_fin)

          final_cov_back <- coco_select_forw[!(coco_select_forw) %in% coco_select_back_fin] # covariate not in the base relationship

          final_cov <- coco_select_forw_tot[!(coco_select_forw_tot) %in% coco_select_back_fin] # includes covariate of the base relationship

          print(glue::glue_col('{col_red Backward step completed}'))

          print(glue::glue_col('{col_green covariate(s) {glue::glue_collapse(coco_select_back,sep=", ",last=" and ")} removed}'))

        }  else {
          print(glue::glue_col('{col_green No covariate removed}'))

          final_cov_back <- coco_select_forw# covariate not in the base relationship
          final_cov <- coco_select_forw_tot # includes covariate of the base relationship
          print(glue::glue_col('{col_red Backward step completed}'))

        }


        if (!rlang::is_empty(final_cov_back) & is.null(cov_base)) {
          print(glue::glue_col('{col_blue final model includes {glue::glue_collapse(final_cov,sep=", ",last=" and ")} }'))

          print(tabtot)

          final_cov_reg <- glue::glue_collapse(final_cov, sep = "+")

        } else if (rlang::is_empty(final_cov_back) & is.null(cov_base)) {

          final_cov_reg <- 1  ## or final_cov_reg <- 'variable_to_keep_if_needed'
          print(glue::glue_col('{col_blue final model does not include any covariate}'))

        } else if (!rlang::is_empty(final_cov_back) & !is.null(cov_base)){

          print(glue::glue_col('{col_blue final model includes {glue::glue_collapse(final_cov_back,sep=", ",last=" and ")} on top of the base relationship}'))

          print(tabtot)

          final_cov_reg <- glue::glue_collapse(final_cov, sep = "+")

        } else if (rlang::is_empty(final_cov_back) & !is.null(cov_base)) {

          final_cov_reg <- base_relation  ## or final_cov_reg <- 'variable_to_keep_if_needed'
          print(glue::glue_col('{col_blue final model does not include any additional covariate}'))

        }


        if (regression=='lm') final_mod <- glm(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset)
        if (regression=='logistic') final_mod <- glm(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset,family='binomial')
        if (regression=='coxph') final_mod <- survival::coxph(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{final_cov_reg}')),data=dataset)
        if (regression=='ordered-categorical') {
          if (!is.null(weights_ordered)){
            final_mod <- MASS::polr(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset, Hess = T,weights=eval(sym(weights_ordered)))

          } else if (is.null(weights_ordered)) {

            final_mod <- MASS::polr(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset, Hess = T)

          }
        }
        tryCatch({
          print(broom::tidy(final_mod))
        },
        error = function(e){
          return()
        })
        break
      }

      print(glue::glue('covariate {coco_select_back_tmp} removed in step {j}'))
      coco_select_back <- c(coco_select_back,coco_select_back_tmp)
      print(coco_select_back)


      ## to check , probably already covered by other conditions
      #  if (i==length(coco_select_forw_tot)){
      #
      #    final_cov_reg <- glue::glue_collapse(final_cov, sep = "+")

      #    if (regression=='lm') final_mod <- glm(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset)
      #    if (regression=='logistic') final_mod <- glm(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset,family='binomial')
      #    if (regression=='coxph') final_mod <- survival::coxph(as.formula(glue::glue(' survival::Surv({variable}, {variable_event})~{final_cov_reg}')),data=dataset)
      #    if (regression=='ordered-categorical') {
      #  if (!is.null(weights_ordered)){
      #  final_mod <- MASS::polr(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset, Hess = T,weights=eval(sym(weights_ordered)))
      #  } else if (is.null(weights_ordered)){
      #    final_mod <- MASS::polr(as.formula(glue::glue('{variable} ~ {final_cov_reg}')),data=dataset, Hess = T)
      #
      #}
      #  }
      #    print(broom::tidy(final_mod))


      #  }
      ####################
    }
  } else if (is.null(coco_select_forw)){

    print('No covariate included in the forward step, no backward step performed')
    final_mod <- reg.full
    tryCatch({
      print(broom::tidy(final_mod))
    },
    error = function(e){
      return()
    })
    final_cov <- coco_select_forw
  }

  tabtot <- tabtot %>%
    dplyr::mutate(term=ifelse(term=='<none>','reference',term))

  ## Output
  p.forw <- p.back <- NULL

  if (test_used=='Chisq'){
    p.forw <- p_forward
    p.back <- p_backward

  }

  scmobject <- list(scmlog=tabtot,test=test_used, regression=regression, final_mod=final_mod, forward_mod=reg.full.forw,forward_inter_mod=reg.full.int,
  forward_cov=forward_cov,forward_int_cov=forward_int_cov,final_cov=final_cov, rem_cov=coco_select_back, p_forw=p.forw, p_back=p.back)

  attr(scmobject, "class") <- "scmobject"

  return(scmobject)

}

sym <- rlang::sym
`%>%` <- magrittr::`%>%`
