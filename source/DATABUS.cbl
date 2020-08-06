       Identification Division.

       Program-Id. DATABUS.
       Author.     Circle Computer Group LLC.

           Remarks. This program is the Business Logic for
                   CICS Application Programming Workshop class.
                   This program LINKed to by DATAPGM.
                   Instead of using a COMMAREA interface it uses
                   a simple example of CHANNELS and CONTAINERS

       Environment Division.
       Data Division.

       Working-Storage Section.

        COPY PAYCOM1.

       01  variables.
           02  ws-channel-name           pic x(16).
           02  ws-field-ind              pic x.
           02  ws-time                   pic s9(15) comp-3.
           02  ws-time-stamp redefines ws-time pic x(8).
           02  ws-item                 pic S9(4) comp value 0.
           02  ws-remainder            pic S9(4) comp value 0.
           02  ws-numitems             pic S9(4) comp value 0.

       01  ws-line-count               pic s9(4) comp.
       01  ws-check-dept               pic x.
       01  ws-item-work                pic s9(4) comp.
       01  ws-response                 pic s9(8) comp value 0.

       01  ws-divide-fields.
           02  ws-div-field            pic s99.
           02  ws-rem-field            pic s99.

       01  ws-page-num                 pic s99.

       01  ws-lineout.
          02  ws-empl-no               pic x(5).
          02  ws-empl-name             pic x(20).
          02  ws-empl-phone            pic x(8).

           copy payroll.

       Procedure Division.

           move spaces to ws-payroll-data
           move zeros  to ws-l-count
                          ws-l-tot
                          ws-item-no

           exec cics assign channel(ws-channel-name)
           end-exec

           exec cics get container('request-cont')
                         channel(ws-channel-name)
                         into(ws-payroll-data)
           end-exec

           move spaces to ws-msg

           evaluate ws-request
               when 'DISP'
                   perform DISPLAYS
               when 'UPDT'
                   perform UPDATES
               when 'ADDS'
                   perform ADDS
               when 'DLET'
                   perform DELETES
               when 'BACK'
                   perform BROWSE-BACK
               when 'FWRD'
                   perform BROWSE-FORWARD
               when 'DBRW'
                   perform DEPT-BROWSE
               when 'DBCK'
                   perform DEPT-BACK
               when 'DFWD'
                   perform DEPT-FWRD
               when 'RETN'
                   perform EMPL-BACK
               when 'DELQ'
                   perform DELETE-TSQ
               when other
                   perform ERROR-ROUTINE
           end-evaluate

           perform return-back

           exit.
      * this is the Payroll display

       DISPLAYS.

           exec cics read file('PAYROLL')
                          ridfld(ws-key)
                          into(payroll-record)
                          nohandle
           end-exec

           if eibresp not = dfhresp(NORMAL)
             move 'No such Record' to ws-msg
             move 'y' to ws-error-ind
           else

             move pr-department            to ws-department
             move pr-employee-no           to ws-employee-no
             move pr-name                  to ws-name
             move pr-addr1                 to ws-addr1
             move pr-addr2                 to ws-addr2
             move pr-addr3                 to ws-addr3
             move pr-phone-no              to ws-phone-no
             move pr-time-stamp            to ws-timestamp
             move pr-salary                to ws-salary
             move pr-start-date            to ws-start-date
             move pr-remarks               to ws-remarks
           end-if
           exit.

      * this is the Payroll update

       UPDATES.

      * First time through just displays the record

           if ws-update-ind = 'n'
              move 'y' to ws-update-ind
              move 'Please make your changes and hit PF4' to ws-msg
              move 'n' to ws-error-ind
              perform DISPLAYS
              perform return-back
           end-if

      * Second time through updates the record

           move 'n' to ws-update-ind

           exec cics read file('PAYROLL')
                          ridfld(ws-key)
                          into(payroll-record)
                          update
                          nohandle
           end-exec

           if eibresp not = dfhresp(NORMAL)
              move 'Problem with that record' to ws-msg
              move 'y' to ws-error-ind

              perform return-back
           end-if

           if ws-upd-name = 'y'
              move ws-name to pr-name
           end-if

           if ws-upd-addr1 = 'y'
              move ws-addr1 to pr-addr1
           end-if

           if ws-upd-addr2 = 'y'
              move ws-addr2 to pr-addr2
           end-if

           if ws-upd-addr3 = 'y'
              move ws-addr3 to pr-addr3
           end-if

           if ws-upd-phone-no = 'y'
              move ws-phone-no to pr-phone-no
           end-if

           if ws-upd-salary = 'y'
              move ws-salary to pr-salary
           end-if

           if ws-upd-start-date = 'y'
              move ws-start-date to pr-start-date
           end-if

           if ws-upd-remarks = 'y'
              move ws-remarks to pr-remarks
           end-if

           exec cics asktime abstime(ws-time)
           end-exec

           move ws-time-stamp to pr-time-stamp

           exec cics rewrite file('PAYROLL')
                             from(payroll-record)
                             nohandle
           end-exec

           if eibresp not = dfhresp(NORMAL)
              move 'y' to ws-error-ind
              move 'Failure on record update' to ws-msg
           else
              move 'Update successful' to ws-msg
           end-if

           exit.

       ADDS.

      * Add new employee record
      * first time through ask for details and confirmation

           if ws-add-ind = 'n'
              exec cics read file('PAYROLL')
                             ridfld(ws-key)
                             into(payroll-record)
                             nohandle
              end-exec

              if eibresp = dfhresp(NORMAL)
                 move 'Record already on file' to ws-msg
                 move 'y' to ws-error-ind
              else
                 move spaces to ws-name
                                ws-addr1
                                ws-addr2
                                ws-addr3
                                ws-phone-no
                                ws-timestamp
                                ws-salary
                                ws-start-date
                                ws-remarks
                 move 'y' to ws-add-ind
                 move 'Enter new employee details and hit PF5' to
                      ws-msg
              end-if
           else

      * secondtime through do the add

              move 'n'                    to ws-add-ind
              move ws-department          to pr-department
              move ws-employee-no         to pr-employee-no
              move ws-name                to pr-name
              move ws-addr1               to pr-addr1
              move ws-addr2               to pr-addr2
              move ws-addr3               to pr-addr3
              move ws-phone-no            to pr-phone-no
              move ws-salary              to pr-salary
              move ws-start-date          to pr-start-date
              move ws-remarks             to pr-remarks
              move 'Employee added succesfully' to ws-msg
              exec cics asktime abstime(ws-time)
              end-exec

              move ws-time-stamp           to pr-time-stamp

              exec cics write file('PAYROLL')
                              ridfld(ws-key)
                              from(payroll-record)
                              nohandle
              end-exec


              if eibresp not = dfhresp(NORMAL)
                move 'Add failed' to ws-msg
                move 'y' to ws-error-ind
              end-if
           end-if
           exit.

       DELETES.

           if ws-delete-ind = 'n'
             move 'y' to ws-delete-ind
             move 'Hit PF6 to confirm delete' to ws-msg
             perform displays
             perform return-back
           else

             move 'n' to ws-delete-ind
             exec cics read file('PAYROLL')
                            ridfld(ws-key)
                            into (payroll-record)
                            update
                            nohandle
             end-exec

             if eibresp not = dfhresp(NORMAL)
               move 'Error reading Payroll record' to ws-msg
               move 'y' to ws-error-ind

               perform return-back
             end-if

             exec cics delete file('PAYROLL')
                              nohandle
             end-exec

             if eibresp not = dfhresp(NORMAL)
               move 'Error deleting Payroll file' to ws-msg
               move 'y' to ws-error-ind
             else
               move 'Record deleted successfully' to ws-msg
             end-if

           exit.

       BROWSE-BACK.

           exec cics startbr file('PAYROLL')
                             ridfld(ws-key)
                             GTEQ
                             nohandle
           end-exec

           if eibresp = dfhresp(NOTFND)
               move 'You have reached the front of the file' to ws-msg
               move 'n' to ws-error-ind

               perform return-back
           end-if

           move 'y' to ws-browse-ind

           exec cics readprev file('PAYROLL')
                              into(payroll-record)
                              ridfld(ws-key)
                              nohandle
           end-exec


           exec cics readprev file('PAYROLL')
                              into(payroll-record)
                              ridfld(ws-key)
                              nohandle
           end-exec

           if eibresp = dfhresp(ENDFILE)
               move 'You have reached the front of the file' to ws-msg
               move 'n' to ws-error-ind

               perform return-back
           end-if

           exec cics endbr file('PAYROLL')
           end-exec

           perform DISPLAYS
           perform return-back
           exit.

       BROWSE-FORWARD.

           exec cics startbr file('PAYROLL')
                             ridfld(ws-key)
                             GTEQ
                             nohandle
           end-exec
           if eibresp = dfhresp(NOTFND)
               move 'You have reached the end of the file' to ws-msg
               move 'n' to ws-error-ind

               perform return-back
           end-if

           move 'y' to ws-browse-ind

           exec cics readnext file('PAYROLL')
                              into(payroll-record)
                              ridfld(ws-key)
                              nohandle
           end-exec

           exec cics readnext file('PAYROLL')
                              into(payroll-record)
                              ridfld(ws-key)
                              nohandle
           end-exec

           if eibresp = dfhresp(ENDFILE)
               move 'You have reached the end of the file' to ws-msg
               move 'n' to ws-error-ind

               perform return-back
           end-if

           perform DISPLAYS
           perform return-back
           exit.

      **********************************************************
      * PF9 Processing  (Department Browse)                    *
      **********************************************************
       DEPT-BROWSE.

           exec cics deleteq ts queue(ws-browseq)
                             nohandle
           end-exec

           exec cics startbr file('PAYROLL')
                             ridfld(ws-key)
                             generic
                             keylength(1)
                             equal
                             resp(ws-response)
           end-exec
           if ws-response = dfhresp(NOTFND) then
              move 'Enter a valid Dept and press F9 again' to ws-msg
              move 'n' to ws-error-ind
              perform return-back
           end-if
           exec cics readnext file('PAYROLL')
                              ridfld(ws-key)
                              into(payroll-record)
           end-exec
           if pr-department not = ws-department then
              move 'No employees in this dept.' to ws-msg
              move 'n' to ws-error-ind
              perform return-back
           end-if
           move ws-department to ws-check-dept
           perform
             until ((ws-response = dfhresp(ENDFILE)) or
              pr-department not = ws-check-dept)
              move pr-employee-no to ws-empl-no
              move pr-name        to ws-empl-name
              move pr-phone-no    to ws-empl-phone
              exec cics writeq ts queue(ws-browseq)
                                  from(ws-lineout)
                                  item(ws-l-tot)
              end-exec
              exec cics readnext file('PAYROLL')
                                 ridfld(ws-key)
                                 into(payroll-record)
                                 resp(ws-response)
              end-exec
           end-perform
           move ws-check-dept to ws-department
           exec cics endbr file('PAYROLL')
           end-exec

              perform varying ws-line-count from 1 by 1
              until ws-line-count > 10
                move spaces to ws-line-out(ws-line-count)
              end-perform


           perform varying ws-line-count from 1 by 1
              until ws-response = dfhresp(ITEMERR) or
                  ws-line-count > 10
                 exec cics readq ts queue(ws-browseq)
                           into(ws-line-out(ws-line-count))
                           resp(ws-response)
                 end-exec
           end-perform

           divide ws-l-tot by 10 giving ws-div-field
              remainder ws-rem-field
           if ws-rem-field > 0 then
              add 1 to ws-div-field
           end-if
           move ws-div-field to ws-sbtot
           move 01 to ws-sbnum

           move 'y' to ws-dept-ind

           move 1 to ws-l-count
           move 10 to ws-item-no
           exit.

      **********************************************************
      * PF10 Processing (Department Browse backward)           *
      **********************************************************
       DEPT-BACK.

              if ws-l-count = 1 then
                 move 'at first page' to ws-msg
                 move 's' to ws-error-ind
                 perform return-back
              end-if
              subtract 10 from ws-l-count
              subtract 1 from ws-l-count giving ws-item-no
              perform varying ws-line-count from 1 by 1
              until ws-response = dfhresp(ITEMERR) or
                  ws-line-count > 10
                 add ws-line-count to ws-item-no giving ws-item-work
                 exec cics readq ts queue(ws-browseq)
                                    into(ws-line-out(ws-line-count))
                                    item(ws-item-work)
                                    resp(ws-response)
                 end-exec
              end-perform
              move ws-item-work to ws-item-no
              subtract 1 from ws-l-count giving ws-div-field
              divide ws-div-field by 10 giving ws-page-num
                 remainder ws-rem-field
              add 1 to ws-page-num
              move ws-page-num to ws-sbnum
              divide ws-l-tot by 10 giving ws-div-field
                 remainder ws-rem-field
              if ws-rem-field > 0 then
                 add 1 to ws-div-field
              end-if
              move ws-div-field to ws-sbtot
              move 'y' to ws-dept-ind
              perform return-back.

      **********************************************************
      * PF11 Processing (Department Browse forward)            *
      **********************************************************
       DEPT-FWRD.

              if ws-l-tot not > (ws-l-count + 9) then
                 move 'at last page' to ws-msg
                 move 's' to ws-error-ind
                 perform return-back
              end-if
              add 10 to ws-l-count

              perform varying ws-line-count from 1 by 1
              until ws-line-count > 10
                move spaces to ws-line-out(ws-line-count)
              end-perform

              perform varying ws-line-count from 1 by 1
              until ws-response = dfhresp(ITEMERR) or
                  ws-line-count > 10
                 add ws-line-count to ws-item-no giving ws-item-work
                 exec cics readq ts queue(ws-browseq)
                                    into(ws-line-out(ws-line-count))
                                    item(ws-item-work)
                                    resp(ws-response)
                 end-exec
              end-perform

              move ws-item-work to ws-item-no
              subtract 1 from ws-l-count giving ws-div-field
              divide ws-div-field by 10 giving ws-page-num
                 remainder ws-rem-field
              add 1 to ws-page-num
              move ws-page-num to ws-sbnum
              divide ws-l-tot by 10 giving ws-div-field
                 remainder ws-rem-field
              if ws-rem-field > 0 then
                 add 1 to ws-div-field
              end-if
              move ws-div-field to ws-sbtot
              move 'y' to ws-dept-ind
              perform return-back.

      **********************************************************
       EMPL-BACK.

           if ws-request = 'RETN' then
              if eibcposn < 640 or
                 eibcposn > 1439
                 exec cics deleteq ts queue(ws-browseq)
                                      resp(ws-response)
                 end-exec
                 move 'n' to ws-dept-ind
                 perform return-back
              else
                 subtract 639 from eibcposn giving ws-item

                 divide ws-item by 80 giving ws-item
                     remainder ws-remainder
                 add ws-l-count to ws-item
                 exec cics readq ts queue(ws-browseq)
                                    into(ws-lineout)
                                    item(ws-item)
                                    resp(ws-response)
                 end-exec
                 if ws-response = dfhresp(ITEMERR)
                    exec cics deleteq ts queue(ws-browseq)
                    end-exec
                    move 'n' to ws-dept-ind
                    move 'n' to ws-error-ind
                    perform return-back
                 end-if
           move ws-empl-no to ws-employee-no
           exec cics read file('PAYROLL')
                          ridfld(ws-key)
                          into(payroll-record)
                          nohandle
           end-exec.
             move pr-department            to ws-department
             move pr-employee-no           to ws-employee-no
             move pr-name                  to ws-name
             move pr-addr1                 to ws-addr1
             move pr-addr2                 to ws-addr2
             move pr-addr3                 to ws-addr3
             move pr-phone-no              to ws-phone-no
             move pr-time-stamp            to ws-timestamp
             move pr-salary                to ws-salary
             move pr-start-date            to ws-start-date
             move pr-remarks               to ws-remarks
           exec cics deleteq ts queue(ws-browseq)
                                nohandle
           end-exec
           move 'n' to ws-dept-ind
           move 'n' to ws-error-ind
           perform return-back.

       ERROR-ROUTINE.
           move 'Invalid request' to ws-msg
           move 'y' to ws-error-ind
           perform return-back.

       DELETE-TSQ.

           exec cics deleteq ts
                     queue(ws-browseq)
                     resp(ws-response)
           end-exec

           perform return-back.

      **********************************************************
      * return section                                         *
      **********************************************************

       return-back.

           exec cics put container('request-cont')
                         channel(ws-channel-name)
                         from(ws-payroll-data)
           end-exec

           exec cics return
           end-exec
           exit.

