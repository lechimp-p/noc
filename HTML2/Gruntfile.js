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

    // FTP config
    var ftp_conf = JSON.parse(fs.readFileSync('./ftp.json', 'utf-8'));

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
        { 'bs-css' :
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
            }
        , 'ng-css' :
            { expand : true
            , cwd : 'bower_components/angular'
            , src : 'angular-csp.css'
            , dest : 'dist/css'
            }
        , 'ng-js' : { files : [
            { expand : true
            , cwd : 'bower_components/angular'
            , src : 'angular.min.js'
            , dest : 'dist/js'
            },
            { expand : true
            , cwd : 'bower_components/angular'
            , src : 'angular.min.js.map'
            , dest : 'dist/js'
            }]}
        , 'ng-bs-js' :
            { expand : true
            , cwd : 'bower_components/angular-bootstrap'
            , src : '*.min.js'
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
            , dest : 'dist/css'
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
        },

        watch :
        { 'custom-html' :
            { files : ['custom/*.html']
            , tasks : ['copy:custom-html', 'ftp-deploy']
            }
        , 'custom-js' :
            { files : ['custom/js/*.js']
            , tasks : ['copy:custom-html', 'ftp-deploy']
            }
        , 'custom-css' :
            { files : ['custom/*.css']
            , tasks : ['copy:custom-css', 'ftp-deploy']
            }
        , 'custom-fonts' :
            { files : ['custom/*']
            , tasks : ['copy:custom-fonts', 'ftp-deploy']
            }
        , 'custom-img' :
            { files : ['custom/img/*']
            , tasks : ['copy:custom-img', 'ftp-deploy']
            }
        },

        'ftp-deploy' :
        { urbanoid :
            { auth : ftp_conf
            , src : ftp_conf.src
            , dest : ftp_conf.desct
            , authKey : ftp_conf.authKey
            }
        }
    });

    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-ftp-deploy');

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
    grunt.registerTask('dist-js', ['copy:custom-js', 'copy:ng-js', 'copy:ng-bs-js']);

    // CSS distribution task.
    grunt.registerTask('dist-css', ['copy:bs-css', 'copy:ng-css', 'copy:custom-css']);

    // IMG distribution task.
    grunt.registerTask('dist-img', ['copy:custom-img']);

    // FONT distribution task.
    grunt.registerTask('dist-fonts', ['copy:bs-fonts', 'copy:custom-fonts']);

    // HTML distribution task.
    grunt.registerTask('dist-html', ['copy:custom-html']);

    // Full distribution task.
    grunt.registerTask('dist', ['clean', 'dist-css', 'dist-js', 'dist-img', 'dist-fonts', 'dist-html']);

    grunt.registerTask('dist-custom', ['copy:custom-js', 'copy:custom-css', 'copy:custom-img', 'copy:custom-fonts', 'copy:custom-html']);

    grunt.registerTask('dist-full', ['run-bootstrap-grunt', 'dist']);

    // Default task.
    //grunt.registerTask('default', ['dist']);
};
