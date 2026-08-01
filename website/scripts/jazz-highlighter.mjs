import {createHighlighterCoreSync} from 'shiki/core';
import {createJavaScriptRegexEngine} from 'shiki/engine/javascript';
import dracula from 'shiki/themes/dracula.mjs';
import githubLight from 'shiki/themes/github-light.mjs';

import jazzTextMateGrammar from '../../editors/vscode-jazz/syntaxes/jazz.tmLanguage.json' with {type: 'json'};

const jazzLanguage = {
  ...jazzTextMateGrammar,
  name: 'jazz',
  aliases: ['Jazz'],
};

const highlighter = createHighlighterCoreSync({
  engine: createJavaScriptRegexEngine(),
  langs: [jazzLanguage],
  themes: [githubLight, dracula],
});

export function tokenizeJazz(code, colorMode, options = {}) {
  return highlighter.codeToTokens(code, {
    lang: 'jazz',
    theme: colorMode === 'dark' ? 'dracula' : 'github-light',
    includeExplanation: options.includeExplanation ?? false,
  });
}
