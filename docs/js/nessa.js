document.addEventListener('DOMContentLoaded', _ => {
    hljs.registerLanguage('nessa', function () {
        return {
            case_insensitive: false, // language is case-insensitive
            keywords: {
                keyword: 'if else for while fn op return syntax type class let',
                literal: ['false', 'true'],
            },
            contains: [
                {
                    className: 'literal',
                    begin: '\'[a-zA-Z_0-9]+'
                },

                {
                    className: 'type',
                    begin: '[A-Z]+[a-zA-Z_0-9]*'
                },

                {
                    className: 'title',
                    begin: '[a-zA-Z_0-9]+(?=\\s*:)'
                },

                {
                    className: 'string',
                    begin: '"',
                    end: '"',
                    contains: [
                        {
                            className: 'escape',
                            begin: '\\\\.',
                            relevance: 0
                        }
                    ],
                },

                hljs.COMMENT(
                    '/\\*', // begin
                    '\\*/', // end
                    {
                        contains: [{
                            className: 'doc',
                            begin: '@\\w+'
                        }]
                    }
                ),

                hljs.COMMENT(
                    '//', // begin
                    '$', // end
                    {
                        contains: [{
                            className: 'doc',
                            begin: '@\\w+'
                        }]
                    }
                ),

                {
                    className: 'number',
                    begin: '\\b\\d+(.\\d+)?\\b',
                    relevance: 0
                }
            ]
        }

    })

    hljs.configure({
        languages: ['nessa']
    })

    document.querySelectorAll('code').forEach((block) => {
        if (!block.innerHTML.includes('%% mermaid')) {
            hljs.highlightElement(block);
            hljs.lineNumbersBlock(block);   
        }
    });
});