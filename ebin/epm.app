{application, epm,
 [{description, "An Erlang package manager"},
  {vsn, "0.1.1"},
  {modules, [ epm,
              epm_core,
              epm_package,
              epm_util,
              api_behavior,
              github_api,
              bitbucket_api,
              yaws_html
  ]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, [
    {sasl, [
      {sasl_error_logger, false}
    ]}
  ]}
]}.
