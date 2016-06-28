'use strict';

angular.module('leApp').
  factory('apiService', ['$http', '$cookies', function ($http, $cookies) {
    var apiInfo = {
      //url: 'https://calm-eyrie-6520.herokuapp.com/api',
      url: '/api',
      login: $cookies.getObject('login') || {
        username: null,
        userID: null,
        password: null,
        isAdmin: null,
        isLoggedIn: false
      }
    };
    
    var auth = function () {
      var keyStr = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
      
      var input = apiInfo.login.username + ':' + apiInfo.login.password;
      var output = "";
      var chr1, chr2, chr3 = "";
      var enc1, enc2, enc3, enc4 = "";
      var i = 0;
      
      do {
          chr1 = input.charCodeAt(i++);
          chr2 = input.charCodeAt(i++);
          chr3 = input.charCodeAt(i++);
      
          enc1 = chr1 >> 2;
          enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
          enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
          enc4 = chr3 & 63;
      
          if (isNaN(chr2)) {
              enc3 = enc4 = 64;
          } else if (isNaN(chr3)) {
              enc4 = 64;
          }
      
          output = output +
              keyStr.charAt(enc1) +
              keyStr.charAt(enc2) +
              keyStr.charAt(enc3) +
              keyStr.charAt(enc4);
          chr1 = chr2 = chr3 = "";
          enc1 = enc2 = enc3 = enc4 = "";
      } while (i < input.length);
      
      return "Basic " + output;
    };
    
    var loginService = {
      userID: function () {
        return apiInfo.login.userID;
      },
      username: function () {
        return apiInfo.login.username;
      },
      password: function () {
        return apiInfo.login.password;
      },
      isLoggedIn: function () {
        return apiInfo.login.isLoggedIn;
      },
      isAdmin: function () {
        return apiInfo.login.isAdmin;
      },
      login: function (un,pw) {
        var loginInfo = {
          loginUsername: un,
          loginPassword: pw
        };
        
        return $http.post(apiInfo.url + '/login/confirmation', loginInfo).
          success(function (data) {
            apiInfo.login.username = un;
            apiInfo.login.password = pw;
            apiInfo.login.isLoggedIn = true;
            apiInfo.login.userID = data.userID;
            apiInfo.login.isAdmin = data.isAdmin;
            $cookies.putObject('login', apiInfo.login);
          });
      },
      logout: function () {
        apiInfo.login = {
          username: null,
          userID: null,
          password: null,
          isAdmin: null,
          isLoggedIn: false
        };
        $cookies.remove('login');
      },
      recoverUsername: function (email) {
        var recInfo = {
          emailAddress: email
        };
        
        return $http.post(apiInfo.url + '/login/username-recovery', recInfo, {
          headers: { 'Authorization': auth() }
        });
      },
      passwordResetRequest: function (un) {
        var recInfo = {
          username: un
        };
        
        return $http.post(apiInfo.url + '/login/password-reset-request', recInfo, {
          headers: { 'Authorization': auth() }
        });
      },
      passwordResetConfirm: function (un,code,pw) {
        var confInfo = {
          resetUsername: un,
          resetCode: code,
          resetNewPassword: pw
        };
        
        return $http.post(apiInfo.url + '/login/password-reset-confirmation', confInfo, {
          headers: { 'Authorization': auth() }
        });
      }
    };
    
    var appsService = {
      create: function (name) {
        var appConfig = {
          appNameConfig: name
        };
        
        return $http.post(apiInfo.url + '/apps', appConfig, {
          headers: { 'Authorization': auth() }
        });
      },
      read: function () {
        return $http.get(apiInfo.url + '/apps', {
          headers: { 'Authorization': auth() }
        });
      },
      id: {
        read: function (id) {
          return $http.get(apiInfo.url + '/apps/' + id.toString(), {
            headers: { 'Authorization': auth() }
          });
        },
        update: function (id,info) {
          var appUpdate = {
            appNameUpdate: info.name,
            appDescriptionUpdate: info.description
          };
          
          return $http.put(apiInfo.url + '/apps/' + id.toString(), appUpdate, {
            headers: { 'Authorization': auth() }
          });
        },
        updatePackages: function (id,pkgs) {
          var appUpdate = {
            appPackagesUpdate: pkgs
          };
          
          return $http.put(apiInfo.url + '/apps/' + id.toString(), appUpdate, {
            headers: { 'Authorization': auth() }
          });
        },
        destroy: function (id) {
          return $http.delete(apiInfo.url + '/apps/' + id.toString(), {
            headers: { 'Authorization': auth() }
          });
        },
        tokens: {
          create: function (aid) {
            return $http.post(apiInfo.url + '/apps/' + aid.toString() + '/tokens', null, {
              headers: { 'Authorization': auth() }
            });
          },
          id: {
            destroy: function (aid, tid) {
              return $http.delete(apiInfo.url + '/apps/' + aid.toString() + '/tokens/' + tid.toString(), {
                headers: { 'Authorization': auth() }
              });
            }
          }
        },
        replConversations: {
          create: function (aid) {
            return $http.post(apiInfo.url + '/apps/' + aid.toString() + "/repl-conversations", [], {
              headers: { 'Authorization': auth() }
            });
          },
          id: {
            updateDiscourseMove: function (aid,cid,move) {
              let convoMoveUpdate = {
                "tag": "ConversationUpdateDiscourseMove",
                "move": move
              };
              
              return $http.put(apiInfo.url + '/apps/' + aid.toString() + "/repl-conversations/" + cid.toString(), convoMoveUpdate, {
              headers: { 'Authorization': auth() }
            });
            }
          }
        },
        parseErrors: {
          id: {
            destroy: function (aid,eid) {
              return $http.delete(apiInfo.url + '/apps/' + aid.toString() + '/parse-errors/' + eid.toString(), {
                headers: { 'Authorization': auth() }
              });
            }
          }
        },
        parseErrorCharts: {
          id: {
            read: function (aid,eid) {
              return $http.get(apiInfo.url + '/apps/' + aid.toString() + '/parse-error-charts/' + eid.toString(), {
                headers: { 'Authorization': auth() }
              });
            }
          }
        }
      }
    };
    
    var usersService = {
      create: function (un,email,pw) {
        var signupInfo = {
          usernameConfig: un,
          passwordConfig: pw,
          emailAddressConfig: email
        };
        
        return $http.post(apiInfo.url + '/users', signupInfo, {
          headers: { 'Authorization': auth() }
        });
      },
      id: {
        read: function () {
          return $http.get(apiInfo.url + '/users/' + apiInfo.login.userID.toString(), {
            headers: { 'Authorization': auth() }
          });
        },
        updateEmail: function (newemail) {
          var newEmailInfo = {
            emailAddressUpdate: newemail
          };
          
          return $http.put(apiInfo.url + '/users/' + apiInfo.login.userID.toString(), newEmailInfo, {
            headers: { 'Authorization': auth() }
          });
        },
        updatePassword: function (newpw) {
          var newpwInfo = {
            passwordUpdate: newpw
          };
          
          return $http.put(apiInfo.url + '/users/' + apiInfo.login.userID.toString(), newpwInfo, {
            headers: { 'Authorization': auth() }
          }).success(function () {
            apiInfo.login.password = newpw;
          });
        }
      }
    };
    
    var packagesService = {
      create: function (name) {
        var packageConfig = {
          packageNameConfig: name
        };
        
        return $http.post(apiInfo.url + '/packages', packageConfig, {
          headers: { 'Authorization': auth() }
        });
      },
      read: function () {
        return $http.get(apiInfo.url + '/packages', {
          headers: { 'Authorization': auth() }
        });
      },
      id: {
        read: function (id) {
          return $http.get(apiInfo.url + '/packages/' + id.toString(), {
            headers: { 'Authorization': auth() }
          });
        },
        update: function (id,info) {
          var packageUpdate = {
            packageNameUpdate: info.name,
            packageDescriptionUpdate: info.description,
            packageIsPreludeUpdate: info.isPrelude,
            packageUsesPreludeUpdate: info.usesPrelude,
            packageIsPublicUpdate: info.isPublic
          };
          
          return $http.put(apiInfo.url + '/packages/' + id.toString(), packageUpdate, {
            headers: { 'Authorization': auth() }
          });
        },
        destroy: function (id) {
          return $http.delete(apiInfo.url + '/packages/' + id.toString(), {
            headers: { 'Authorization': auth() }
          });
        },
        build: {
          create: function (id) {
            return $http.put(apiInfo.url + '/packages/' + id.toString() + '/build', null, {
              headers: { 'Authorization': auth() }
            });
          }
        },
        files: {
          create: function (id, name) {
            var fileConfig = {
              fileNameConfig: name
            };
            
            return $http.post(apiInfo.url + '/packages/' + id.toString() + '/files', fileConfig, {
              headers: { 'Authorization': auth() }
            });
          },
          id: {
            read: function (pid,fid) {
              return $http.get(apiInfo.url + '/packages/' + pid.toString() + '/files/' + fid.toString(), {
                headers: { 'Authorization': auth() }
              });
            },
            update: function (pid,fid,info) {
              var fileUpdate = {
                fileNameUpdate: info.name,
                fileDescriptionUpdate: info.description,
                fileSourceCodeUpdate: info.sourceCode
              };
              
              return $http.put(apiInfo.url + '/packages/' + pid.toString() + '/files/' + fid.toString(), fileUpdate, {
                headers: { 'Authorization': auth() }
              });
            },
            destroy: function (pid,fid) {
              return $http.delete(apiInfo.url + '/packages/' + pid.toString() + '/files/' + fid.toString(), {
                headers: { 'Authorization': auth() }
              });
            }
          }
        }
      }
    };
    
    
    return {
      errors: {
        // general errors
        
      },
      error: function (er) {
        return this.errors[er] || ("An error has occurred: " + er);
      },
      login: loginService,
      apps: appsService,
      users: usersService,
      packages: packagesService
    };
  }]);
  