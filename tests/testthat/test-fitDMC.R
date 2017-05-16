context("firDirectCentrality function")

test_that("check errors", {

  # class has to be specified
  expect_error(fitDMC(bartomeus,class=NULL))

  # data has to be class as.alienData
  expect_error(fitDMC(c(1:4),class="rf"))

  # test errors on data missing pieces (individus)
  data_test <- bartomeus
  data_test$traitInd <- NULL
  expect_error(fitDMC(data_test,class="rf",level="individus"))

  # test errors on data missing pieces (species)
  data_test <- bartomeus
  data_test$traitSp <- NULL
  expect_error(fitDMC(data_test,class="rf",level="species"))

})

test_that("check output", {

  # for species
  # test glm output
  model <- fitDMC(bartomeus,class='glm',family=gaussian(),level='species')
  expect_that(model, is_a("glm"))
  expect_equal(model$method,"glm.fit")
  expect_equal(attr(model,"level"), "species")

  # test rf output
  model <- fitDMC(bartomeus,class='rf',level='species')
  expect_that(model, is_a("randomForest"))
  expect_true(!is.null(model$forest))
  expect_equal(attr(model,"level"), "species")


  # for individus
  # test glm output
  model <- fitDMC(bartomeus,class='glm',family=gaussian(),level='individus')
  expect_that(model, is_a("glm"))
  expect_equal(model$method,"glm.fit")
  expect_equal(attr(model,"level"), "individus")

  # test rf output
  model <- fitDMC(bartomeus,class='rf',level='individus')
  expect_that(model, is_a("randomForest"))
  expect_true(!is.null(model$forest))
  expect_equal(attr(model,"level"), "individus")

})
