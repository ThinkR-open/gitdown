globalVariables(
  unique(c(
    "name", "sha", "target", "tag.name", "tag.message", "pattern", "order", ".",
    "pattern_numeric",
    # each_pattern()
    "link_pattern", "data", "pattern.content",
    # get_commit_pattern
    "pattern_extract", "pattern.title", "pattern.content.orig",
    # nest_commits_by_pattern
    "pattern.content", "pattern.type", "text_link", "message_link.type",
    # get_info
    "filepath",
    # get_last_modif
    "filepath",
    # present_files
    "in_repository", "first_modif", "last_modif"
  ))
)
