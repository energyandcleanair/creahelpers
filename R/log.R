log_layout_crea <- logger::layout_glue_generator(format = paste(
  "{crayon::bold(colorize_by_log_level(level, levelr))}",
  "{colorize_by_log_level(msg, levelr)}"
))

logger::log_layout(log_layout_crea)
