do_package_checks()

if (ci_on_travis()) {
  get_stage("install") %>%
    add_step(step_install_github("ThinkR-open/thinkrtemplate"))

  do_pkgdown()
}
