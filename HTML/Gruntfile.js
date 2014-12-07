/*!
 * NoC-frontends's Gruntfile
 * Copyright 2014 Richard Klees
 */

module.exports = function (grunt) {
    'use strict';

    // Force use of Unix newlines
    grunt.util.linefeed = '\n';

    RegExp.quote = function (string) {
        return string.replace(/[-\\^$*+?.()|[\]{}]/g, '\\$&');
    };

    var fs = require('fs');
    var path = require('path');
    var sys = require('sys');
    var exec = require('child_process').exec;

    // Project configuration.
    grunt.initConfig({
        // Metadata.
        pkg: grunt.file.readJSON('package.json'),
        banner: '/*!\n' +
                ' * NoC-frontend v<%= pkg.version %>' +
                ' */\n',
        // Task configuration.
        clean: {
            dist: ['dist']
        },

        copy: 
        { 'ng-css' :
            { expand : true
            , cwd : 'bower_components/angular'
            , src : 'angular-csp.css'
            , dest : 'dist/css'
            }
        , 'ng-js' : { files : [
            { expand : true
            , cwd : 'bower_components/angular'
            , src : 'angular.js'
            , dest : 'dist/js'
            }]}
        , 'ng-route-js' : { files : [
            { expand : true
            , cwd : 'bower_components/angular-route'
            , src : 'angular-route.js'
            , dest : 'dist/js'
            }]}
        , 'jquery' : 
            { expand : true
            , cwd : 'bower_components/jquery/dist'
            , src : 'jquery.min.js'
            , dest : 'dist/js'
            }
        , 'underscore' : 
            { expand : true
            , cwd : 'bower_components/underscore'
            , src : 'underscore-min.js'
            , dest : 'dist/js'
            }
        , 'custom-css' :
            { expand : true
            , cwd : 'custom/css'
            , src : '*.css'
            , dest : 'dist/css'
            , filter : 'isFile'
            }                  
        , 'custom-fonts' :
            { expand : true
            , cwd : 'custom/fonts'
            , src : '*'
            , dest : 'dist/fonts'
            , filter : 'isFile'
            }
        , 'custom-img' :
            { expand : true
            , cwd : 'custom/img'
            , src : '*'
            , dest : 'dist/img'
            , filter : 'isFile'
            }
        , 'custom-js' :
            { expand : true
            , cwd : 'custom/js'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            }
        , 'custom-controllers' :
            { expand : true
            , cwd : 'custom/controllers'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            }
        , 'custom-directives' :
            { expand : true
            , cwd : 'custom/directives'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            }
        , 'custom-partials' :
            { expand : true
            , cwd : 'custom/partials'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            }
        , 'custom-html' : 
            { expand : true
            , cwd : 'custom'
            , src : '*.html'
            , dest : 'dist'
            , filter : 'isFile'
            }
        },


        watch :
        { 'custom-html' :
            { files : ['custom/*.html']
            , tasks : ['copy:custom-html', 'upload-dist']
            }
        , 'custom-js' :
            { files : ['custom/js/*.js']
            , tasks : ['jshint:custom', 'copy:custom-js', 'upload-dist']
            }
        , 'custom-css' :
            { files : ['custom/css/*.css']
            , tasks : ['copy:custom-css', 'upload-dist']
            }
        , 'custom-fonts' :
            { files : ['custom/*']
            , tasks : ['copy:custom-fonts', 'upload-dist']
            }
        , 'custom-img' :
            { files : ['custom/img/*']
            , tasks : ['copy:custom-img', 'upload-dist']
            }
        , 'custom-controllers' :
            { files : ['custom/controllers/*']
            , tasks : ['copy:custom-controllers', 'upload-dist']
            }
        , 'custom-directives' :
            { files : ['custom/directives/*']
            , tasks : ['copy:custom-directives', 'upload-dist']
            }
        , 'custom-partials' :
            { files : ['custom/partials/*']
            , tasks : ['copy:custom-partials', 'upload-dist']
            }
        },

        jshint :
        { options :
            { laxcomma : true
            }
        , custom : [ 'custom/js/*' ]
        , views : [ 'custom/views/*/*.js' ]
        }
    });

    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-contrib-jshint');

    grunt.registerTask('ftp-deploy', function() {
        var res = this.async();
        res();
    })

    grunt.registerTask('setup-self', function() {
        var cb = this.async();
        exec('npm install; bower install;',
        function(error, result, stderr) {
            console.log(result.stdout);
            console.log(stderr);
            cb();
        }); 
    });

    grunt.registerTask('upload-dist', function() {
        var cb = this.async();
        exec("./upload-dist.sh",
        function(error, result, stderr) {
            console.log(result.stdout);
            console.log(stderr);
            cb();
        });
    });

    grunt.registerTask('setup', ['setup-self']);

    // JS distribution task.
    grunt.registerTask('dist-js', ['copy:custom-js', 'copy:ng-js', 'copy:ng-route-js', 'copy:jquery', 'copy:underscore', 'copy:custom-controllers', 'copy:custom-directives']);

    // CSS distribution task.
    grunt.registerTask('dist-css', ['copy:ng-css', 'copy:custom-css']);

    // IMG distribution task.
    grunt.registerTask('dist-img', ['copy:custom-img']);

    // FONT distribution task.
    grunt.registerTask('dist-fonts', ['copy:custom-fonts']);

    // HTML distribution task.
    grunt.registerTask('dist-html', ['copy:custom-html', 'copy:custom-partials']);

    // Full distribution task.
    grunt.registerTask('dist', ['clean', 'dist-css', 'dist-js', 'dist-img', 'dist-fonts', 'dist-html']);

    grunt.registerTask('dist-full', ['run-bootstrap-grunt', 'dist']);

    // Default task.
    //grunt.registerTask('default', ['dist']);
};
