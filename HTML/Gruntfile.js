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
        { /*'bs-css' :
            { expand : true
            , cwd : 'bower_components/bootstrap/dist/css'
            , src : '*.css'
            , dest : 'dist/css'
            , filter : 'isFile'
            }                  
        , 'bs-fonts' :
            { expand : true
            , cwd : 'bower_components/bootstrap/dist/fonts'
            , src : '*'
            , dest : 'dist/fonts'
            , filter : 'isFile'
            }*/
          'ng-css' :
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
            }]}/*,
            { expand : true
            , cwd : 'bower_components/angular'
            , src : 'angular.min.js.map'
            , dest : 'dist/js'
            }]}*/
        /*, 'ng-bs-js' : { files : [
            { expand : true
            , cwd : 'bower_components/angular-bootstrap'
            , src : 'ui-bootstrap.js'
            , dest : 'dist/js'

            },
            { expand : true
            , cwd : 'bower_components/angular-bootstrap'
            , src : 'ui-bootstrap-tpls.js'
            , dest : 'dist/js'
            }]}*/
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
        /*, 'ng-mobile-js' :
            { expand : true
            , cwd : 'bower_components/mobile-angular-ui/dist/js'
            , src : '*.min.js'
            , dest : 'dist/js'
            }
        , 'ng-mobile-css' :
            { expand : true
            , cwd : 'bower_components/mobile-angular-ui/dist/css'
            , src : '*.min.css'
            , dest : 'dist/css'
            }
        , 'ng-mobile-fonts':
            { expand : true
            , cwd : 'bower_components/mobile-angular-ui/dist/fonts'
            , src : '*'
            , dest : 'dist/fonts'
            }*/
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
        , 'custom-html' : 
            { expand : true
            , cwd : 'custom'
            , src : '*.html'
            , dest : 'dist'
            , filter : 'isFile'
            }
        , 'views-js' : { files : [
            { expand : true
            , cwd : 'custom/views/channel'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/channel-overview'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/chat'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/contact-overview'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/topbar'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/profile'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/my-profile'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/error'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/login'
            , src : '*.js'
            , dest : 'dist/js'
            , filter : 'isFile'
            }]}
        , 'views-html' : { files : [
            { expand : true
            , cwd : 'custom/views/channel'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/channel-overview'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/chat'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/contact-overview'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/topbar'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/profile'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/my-profile'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/error'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            },
            { expand : true
            , cwd : 'custom/views/login'
            , src : '*.html'
            , dest : 'dist/partials'
            , filter : 'isFile'
            }]}
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
        , 'views-js' :
            { files : ['custom/views/*/*.js']
            , tasks : ['jshint:views', 'copy:views-js', 'upload-dist']
            }
        , 'views-html' :
            { files : ['custom/views/*/*.html']
            , tasks : ['copy:views-html', 'upload-dist']
            }
        },

        jshint :
        { options :
            { laxcomma : true
            }
        , custom : [ 'custom/js/*' ]
        , views : [ 'custom/views/*/*.js' ]
        },

        /*'ftp-deploy' :
        { urbanoid :
            { auth : 
                { host : 'server01.campusspeicher.de'
                , port : 22
                , authKey : 'nico'
                }
            , authPath : './.ftppass'
            , src : '/Users/Nico/NoC-Server/HTML2/dist'
            , dest : '/httpdocs/noc'
            }
        }*/
    });

    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-contrib-jshint');
    //grunt.loadNpmTasks('grunt-ftp-deploy');

    grunt.registerTask('ftp-deploy', function() {
        var res = this.async();
        res();
    })

    // install bootstrap
    grunt.registerTask('setup-bootstrap', function() {
        var cb = this.async();
        exec('cd bower_components/bootstrap; npm install; bower install;',
        function(error, result, stderr) {
            console.log(result.stdout);
            console.log(stderr);
            cb();
        });
    });

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

    grunt.registerTask('setup', ['setup-self', 'setup-bootstrap']);

    // run gruntfile from bootstrap
    grunt.registerTask('run-bootstrap-grunt', function() {
        var cb = this.async();
        grunt.util.spawn({
            grunt : true,
            args : [],
            opts : {
                cwd: 'bower_components/bootstrap'
            } 
        }, function(error, result, code) {
            console.log(result.stdout);
            cb();
        });
    });

    // collect all js files from bootstrap 
    //grunt.registerTask('bs-js-copy', function () {
    //});

    // collect all css files from bootstrap 
    //grunt.registerTask('bs-css-copy', function () {
    //});

    // collect all css files from bootstrap 
    //grunt.registerTask('bs-img-copy', function () {
    //});

    // JS distribution task.
    grunt.registerTask('dist-js', ['copy:custom-js', 'copy:ng-js', 'copy:ng-route-js', 'copy:jquery', 'copy:underscore', 'copy:views-js']);

    // CSS distribution task.
    grunt.registerTask('dist-css', ['copy:ng-css', 'copy:custom-css']);

    // IMG distribution task.
    grunt.registerTask('dist-img', ['copy:custom-img']);

    // FONT distribution task.
    grunt.registerTask('dist-fonts', ['copy:custom-fonts']);

    // HTML distribution task.
    grunt.registerTask('dist-html', ['copy:custom-html', 'copy:views-html']);

    // Full distribution task.
    grunt.registerTask('dist', ['clean', 'dist-css', 'dist-js', 'dist-img', 'dist-fonts', 'dist-html']);

    grunt.registerTask('dist-custom', ['copy:custom-js', 'copy:custom-css', 'copy:custom-img', 'copy:custom-fonts', 'copy:custom-html']);

    grunt.registerTask('dist-full', ['run-bootstrap-grunt', 'dist']);

    // Default task.
    //grunt.registerTask('default', ['dist']);
};