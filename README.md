# PJ Model Jiffy

Convert pjm model to/from jiffy compatible erlang terms.

## Usage

Add pjm_jiffy as a dependency in `rebar.config`.

    {deps, [{pjm_jiffy, ".*", {git, "git://github.com/3pjgames/pjm.git", {tag, "1.1.0"}}}]}.

- `pjm_jiffy:to_json(Model :: pjm:model()) -> pjm_jiffy:object()`
- `pjm_jiffy:from_json(Json :: pjm_jiffy:object(), Model :: pjm:model()) -> pjm:model()`
