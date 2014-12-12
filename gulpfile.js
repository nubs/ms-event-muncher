(function() {
  'use strict';

  var gulp = require('gulp');
  var nodemon = require('gulp-nodemon');
  var jshint = require('gulp-jshint');
  var stylish = require('jshint-stylish');

  var jsFiles = ['./**/*.js', '!./node_modules/**/*.js'];

  gulp.task('lint', function() {
    return gulp.src(jsFiles)
      .pipe(jshint())
      .pipe(jshint.reporter(stylish))
      .pipe(jshint.reporter('fail'));
  });

  gulp.task('watch', function() {
    return gulp.watch(jsFiles, ['lint']);
  });

  gulp.task('run', function() {
    return nodemon({script: 'brain/stem.js', ext: 'js hox'});
  });

  gulp.task('default', ['lint']);
  gulp.task('dev', ['lint', 'watch', 'run']);
})();
