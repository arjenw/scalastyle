val scalastyleVersion = settingKey[String]("Scalastyle version")
scalastyleVersion := sys.props.getOrElse("scalastyle.version", "1.5.1")

libraryDependencies += "com.beautiful-scala" %% "scalastyle" % scalastyleVersion.value
