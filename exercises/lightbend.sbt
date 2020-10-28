resolvers in ThisBuild += "lightbend-commercial-mvn" at
  "https://repo.lightbend.com/pass/dP2gSK5aIe8CdxY0wEnaWLjEkjzv6chRu1veqDP5r_E-tzbI/commercial-releases"
resolvers in ThisBuild += Resolver.url("lightbend-commercial-ivy",
  url("https://repo.lightbend.com/pass/dP2gSK5aIe8CdxY0wEnaWLjEkjzv6chRu1veqDP5r_E-tzbI/commercial-releases"))(Resolver.ivyStylePatterns)