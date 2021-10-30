#jinja2: trim_blocks: "true", lstrip_blocks: "false"
// https://docs.gradle.org/current/userguide/directory_layout.html
// https://docs.gradle.org/current/userguide/init_scripts.html
// https://docs.gradle.org/current/userguide/declaring_repositories.html
// https://docs.gradle.org/current/userguide/repository_types.html#repository_types
// https://docs.gradle.org/current/dsl/org.gradle.api.artifacts.dsl.RepositoryHandler.html

{#

We have to use `macro` instead of `set` because `set` does not work with `for`
here.  I don't know the reason.

#}
{% macro gradle_repos(repos) %}
repositories {
    {% for repo in repos %}
    {% if repo.type == 'maven' %}
    maven {
        {% if repo.name is defined %}
        name = {{ repo.name | to_json }}
        {% endif %}
        url = uri({{ repo.url | to_json }})
    }
    {% else %}
    {{ repo.type }}()
    {% endif %}
    {% endfor %}
}
{%- endmacro %}

allprojects {
    buildscript {
        {{ gradle_repos(dotfiles_gradle_repos) | indent(width=8) }}
    }
    {{ gradle_repos(dotfiles_gradle_repos) | indent(width=4) }}
}
