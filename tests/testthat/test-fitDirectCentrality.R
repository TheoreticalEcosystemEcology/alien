context("firDirectCentrality function")

test_that("check errors", {

  # class has to be specified
  expect_error(fitDirectCentrality(bartomeus,class=NULL))

  # data has to be class as.alienData
  expect_error(fitDirectCentrality(c(1:4),class="rf"))

  # test errors on data missing pieces (individus)
  data_test <- bartomeus
  data_test$traitInd <- NULL
  expect_error(fitDirectCentrality(data_test,class="rf",level="individus"))

  # test errors on data missing pieces (species)
  data_test <- bartomeus
  data_test$traitSp <- NULL
  expect_error(fitDirectCentrality(data_test,class="rf",level="species"))

})

test_that("check output", {

  # for species
  # test glm output
  model <- fitDirectCentrality(bartomeus,class='glm',family=gaussian(),level='species')
  expect_that(model, is_a("fitDirectCentrality"))
  expect_equal(model$method,"glm.fit")
  expect_equal(attr(model,"level"), "species")

  # test rf output
  model <- fitDirectCentrality(bartomeus,class='rf',level='species')
  expect_that(model, is_a("fitDirectCentrality"))
  expect_true(!is.null(model$forest))
  expect_equal(attr(model,"level"), "species")


  # for individus
  # test glm output
  model <- fitDirectCentrality(bartomeus,class='glm',family=gaussian(),level='individus')
  expect_that(model, is_a("fitDirectCentrality"))
  expect_equal(model$method,"glm.fit")
  expect_equal(attr(model,"level"), "individus")

  # test rf output
  # model <- fitDirectCentrality(bartomeus,class='rf',level='individus')
  # expect_that(model, is_a("fitDirectCentrality"))
  # expect_true(!is.null(model$forest))
  # expect_equal(attr(model,"level"), "individus")

})
