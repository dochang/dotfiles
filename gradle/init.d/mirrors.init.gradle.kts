#jinja2: trim_blocks: "true", lstrip_blocks: "false"
// https://docs.gradle.org/current/userguide/directory_layout.html
// https://docs.gradle.org/current/userguide/init_scripts.html
// https://docs.gradle.org/current/userguide/declaring_repositories.html
// https://docs.gradle.org/current/userguide/repository_types.html#repository_types
// https://docs.gradle.org/current/dsl/org.gradle.api.artifacts.dsl.RepositoryHandler.html

{% set repos %}
repositories {
    {% for repo in dotfiles_gradle_repos %}
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
{%- endset %}

allprojects {
    buildscript {
        {{ repos | indent(width=8) }}
    }
    {{ repos | indent(width=4) }}
}
