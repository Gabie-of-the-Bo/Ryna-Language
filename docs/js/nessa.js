document.addEventListener('DOMContentLoaded', _ => {
    hljs.registerLanguage('nessa', function () {
        return {
            case_insensitive: false, // language is case-insensitive
            keywords: {
                keyword: 'if else for in while fn op return syntax type class let unary binary nary prefix postfix right from to op interface implement import expr block do break continue',
                literal: ['false', 'true'],
            },
            contains: [
                {
                    className: 'literal',
                    begin: '\'[a-zA-Z_0-9]+(?![a-zA-Z_0-9]*\')'
                },

                {
                    className: 'type',
                    begin: '[A-Z]+[a-zA-Z_0-9]*'
                },

                {
                    className: 'literal',
                    begin: '@[a-zA-Z_0-9]+(?![a-zA-Z_0-9]*\')'
                },

                {
                    className: 'title',
                    begin: '[a-zA-Z_][a-zA-Z_0-9]*(?=\\s*:)'
                },

                {
                    className: 'title',
                    begin: '(?<=\\blet\\s*)[a-zA-Z_][a-zA-Z_0-9]*'
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

                {
                    className: 'literal',
                    begin: '\'',
                    end: '\'',
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
                },

                {
                    className: 'number',
                    begin: '\\b0b(0|1)+\\b',
                    relevance: 0
                },

                {
                    className: 'number',
                    begin: '\\b0x([0-9ABCDEF])+\\b',
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