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
        }
    });

    grunt.loadNpmTasks('grunt-contrib-clean');

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
    grunt.registerTask('bs-js-copy', function () {
    });

    // collect all css files from bootstrap 
    grunt.registerTask('bs-css-copy', function () {
    });

    // collect all css files from bootstrap 
    grunt.registerTask('bs-img-copy', function () {
    });

    // collect all css files from bootstrap 
    grunt.registerTask('bs-font-copy', function () {
    });

    // JS distribution task.
    grunt.registerTask('dist-js', ['bs-js-copy']);

    // CSS distribution task.
    grunt.registerTask('dist-css', ['bs-css-copy']);

    // IMG distribution task.
    grunt.registerTask('dist-img', ['bs-img-copy']);

    // FONT distribution task.
    grunt.registerTask('dist-font', ['bs-font-copy']);

    // HTML distribution task.
    grunt.registerTask('dist-html', []);

    // Full distribution task.
    grunt.registerTask('dist', ['run-bootstrap-grunt', 'clean', 'dist-css', 'dist-js', 'dist-img', 'dist-font', 'dist-html']);

    // Default task.
    grunt.registerTask('default', ['dist']);
};
